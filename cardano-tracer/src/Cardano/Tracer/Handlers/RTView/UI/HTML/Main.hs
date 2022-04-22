{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Main
  ( mkMainPage
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (void)
import           Control.Monad.Extra (whenM)
import           Data.Fixed (Pico)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (secondsToNominalDiffTime)
import           System.Time.Extra (sleep)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Bulma
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Body
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.JS.ChartJS
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.UI
import           Cardano.Tracer.Handlers.RTView.Update.Resources
import           Cardano.Tracer.Types

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
    , UI.mkElement "script" # set UI.html chartJSMoment
    , UI.mkElement "script" # set UI.html chartJSAdapter
    , UI.mkElement "script" # set UI.html chartJSPluginZoom
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

  restoreTheme window
  forceSetDefaultChartsSettings

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
