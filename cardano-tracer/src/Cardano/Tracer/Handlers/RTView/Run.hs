{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.Run
  ( runRTView
  , module Cardano.Tracer.Handlers.RTView.State.TraceObjects
  ) where

import           Control.Concurrent.Async (concurrently_)
import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (void)
import           Control.Monad.Extra (whenJust, whenM)
import           Data.Fixed (Pico)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Clock (secondsToNominalDiffTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Body
import           Cardano.Tracer.Handlers.RTView.UI.JS.ChartJS
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.UI
import           Cardano.Tracer.Handlers.RTView.Update.Historical
import           Cardano.Tracer.Handlers.RTView.Update.Resources
import           Cardano.Tracer.Types

-- | RTView is a part of 'cardano-tracer' that provides an ability
--   to monitor Cardano nodes in a real-time. The core idea is simple:
--   RTView periodically receives some informations from the connected
--   node(s) and displays that information on a web-page.
--
--   The web-page is built using 'threepenny-gui' library. Please note
--   Gitub-version of this library is used, not Hackage-version!
--
--   TODO ...

runRTView
  :: TracerConfig
  -> ConnectedNodes
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> DataPointRequestors
  -> IO ()
runRTView TracerConfig{logging, network, hasRTView, ekgRequestFreq}
          connectedNodes acceptedMetrics savedTO dpRequestors =
  whenJust hasRTView $ \(Endpoint host port) -> do
    -- Initialize displayed stuff outside of main page renderer,
    -- to be able to update corresponding elements after page reloading.
    displayedElements <- initDisplayedElements
    reloadFlag <- initPageReloadFlag
    -- We have to collect different information from the node and save it
    -- independently from RTView web-server. As a result, we'll be able to
    -- show charts with historical data (where X axis is the time) for the
    -- period when RTView web-page wasn't opened.
    resourcesHistory <- initResourcesHistory
    lastResources <- initLastResources
    concurrently_
      (UI.startGUI (config host port) $
         mkMainPage
           connectedNodes
           displayedElements
           savedTO
           dpRequestors
           reloadFlag
           ekgRequestFreq
           logging
           network
           resourcesHistory)
      (runHistoricalUpdater
         savedTO
         acceptedMetrics
         resourcesHistory
         lastResources)
 where
  config h p = UI.defaultConfig
    { UI.jsPort = Just . fromIntegral $ p
    , UI.jsAddr = Just . encodeUtf8 . T.pack $ h
    }

mkMainPage
  :: ConnectedNodes
  -> DisplayedElements
  -> SavedTraceObjects
  -> DataPointRequestors
  -> PageReloadedFlag
  -> Maybe Pico
  -> NonEmpty LoggingParams
  -> Network
  -> ResourcesHistory
  -> UI.Window
  -> UI ()
mkMainPage connectedNodes displayedElements savedTO
           dpRequestors reloadFlag ekgFreq loggingConfig
           networkConfig resourcesHistory window = do
  void $ return window # set UI.title pageTitle
  void $ UI.getHead window #+
    [ UI.link # set UI.rel "icon"
              # set UI.href ("data:image/svg+xml;base64," <> faviconSVGBase64)
    , UI.meta # set UI.name "viewport"
              # set UI.content "width=device-width, initial-scale=1"
    , UI.mkElement "style" # set UI.html bulmaCSS
    , UI.mkElement "style" # set UI.html bulmaTooltipCSS
    , UI.mkElement "style" # set UI.html bulmaPageloaderCSS
    , UI.mkElement "style" # set UI.html ownCSS
    , UI.mkElement "script" # set UI.html chartJS
    -- , UI.mkElement "script" # set UI.html chartJSAdapter
    ]

  pageBody <- mkPageBody window networkConfig

  colors <- initColors
  datasetIndices <- initDatasetsIndices
  datasetTimestamps <- initDatasetsTimestamps

  -- Prepare and run the timer, which will hide the page preloader.
  preloaderTimer <- UI.timer # set UI.interval 10
  on UI.tick preloaderTimer . const $ do
    liftIO $ sleep 0.8
    findAndSet (set UI.class_ "pageloader") window "preloader"
    UI.stop preloaderTimer
  UI.start preloaderTimer

  whenM (liftIO $ readTVarIO reloadFlag) $ do
    updateUIAfterReload
      window
      connectedNodes
      displayedElements
      dpRequestors
      loggingConfig
      colors
      datasetIndices
    liftIO $ pageWasNotReload reloadFlag

  -- Prepare and run the timer, which will call 'update' function every second.
  uiUpdateTimer <- UI.timer # set UI.interval 1000
  on UI.tick uiUpdateTimer . const $
    updateUI
      window
      connectedNodes
      displayedElements
      savedTO
      dpRequestors
      loggingConfig
      colors
      datasetIndices
  UI.start uiUpdateTimer

  -- The user can setup EKG request frequency (in seconds) in tracer's configuration,
  -- so we start resources metrics updating in a separate timer with corresponding interval.
  let toMs dt = fromEnum dt `div` 1000000000
      ekgIntervalInMs = toMs . secondsToNominalDiffTime $ fromMaybe 1.0 ekgFreq
  uiUpdateResourcesTimer <- UI.timer # set UI.interval ekgIntervalInMs
  on UI.tick uiUpdateResourcesTimer . const $
    updateResourcesCharts
      connectedNodes
      resourcesHistory
      datasetIndices
      datasetTimestamps
  UI.start uiUpdateResourcesTimer

  on UI.disconnect window . const $ do
    -- The connection with the browser was dropped (probably user closed the tab),
    -- so timers should be stopped.
    UI.stop uiUpdateTimer
    UI.stop uiUpdateResourcesTimer
    -- To restore current displayed state after DOM-rerendering.
    liftIO $ pageWasReload reloadFlag

  void $ UI.element pageBody
