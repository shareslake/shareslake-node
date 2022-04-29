{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Main
  ( mkMainPage
  ) where

import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (void)
import           Control.Monad.Extra (whenM)
import           Data.List.NonEmpty (NonEmpty)
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
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.Chain
import           Cardano.Tracer.Handlers.RTView.Update.UI
import           Cardano.Tracer.Types

mkMainPage
  :: ConnectedNodes
  -> DisplayedElements
  -> SavedTraceObjects
  -> DataPointRequestors
  -> PageReloadedFlag
  -> NonEmpty LoggingParams
  -> Network
  -> ResourcesHistory
  -> BlockchainHistory
  -> UI.Window
  -> UI ()
mkMainPage connectedNodes displayedElements savedTO
           dpRequestors reloadFlag loggingConfig
           networkConfig resourcesHistory chainHistory window = do
  void $ return window # set UI.title pageTitle
  void $ UI.getHead window #+
    [ UI.link # set UI.rel "icon"
              # set UI.href ("data:image/svg+xml;base64," <> faviconSVGBase64)
    , UI.meta # set UI.name "viewport"
              # set UI.content "width=device-width, initial-scale=1"
    , UI.mkElement "style"  # set UI.html bulmaCSS
    , UI.mkElement "style"  # set UI.html bulmaTooltipCSS
    , UI.mkElement "style"  # set UI.html bulmaPageloaderCSS
    , UI.mkElement "style"  # set UI.html ownCSS
    , UI.mkElement "script" # set UI.html chartJS
    , UI.mkElement "script" # set UI.html chartJSLuxon
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
  restoreChartsSettings

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

  -- For better performance, we update charts only few times per minute.
  uiUpdateChartsTimer <- UI.timer # set UI.interval (15 * 1000)
  on UI.tick uiUpdateChartsTimer . const $ do
    updateBlockchainCharts
      connectedNodes
      chainHistory
      datasetIndices
      datasetTimestamps
  UI.start uiUpdateChartsTimer

  let per = 15 * 1000
      ResHistory rHistory = resourcesHistory

  cpuTimer <-
    mkChartTimer connectedNodes per rHistory datasetIndices datasetTimestamps CPUData CPUChart
  memoryTimer <-
    mkChartTimer connectedNodes per rHistory datasetIndices datasetTimestamps MemoryData MemoryChart
  gcMajorNumTimer <-
    mkChartTimer connectedNodes per rHistory datasetIndices datasetTimestamps GCMajorNumData GCMajorNumChart
  gcMinorNumTimer <-
    mkChartTimer connectedNodes per rHistory datasetIndices datasetTimestamps GCMinorNumData GCMinorNumChart
  gcLiveBytesTimer <-
    mkChartTimer connectedNodes per rHistory datasetIndices datasetTimestamps GCLiveMemoryData GCLiveMemoryChart
  cpuTimeGCTimer <-
    mkChartTimer connectedNodes per rHistory datasetIndices datasetTimestamps CPUTimeGCData CPUTimeGCChart
  cpuTimeAppTimer <-
    mkChartTimer connectedNodes per rHistory datasetIndices datasetTimestamps CPUTimeAppData CPUTimeAppChart
  threadsNumTimer <-
    mkChartTimer connectedNodes per rHistory datasetIndices datasetTimestamps ThreadsNumData ThreadsNumChart

  UI.start cpuTimer
  UI.start memoryTimer
  UI.start gcMajorNumTimer
  UI.start gcMinorNumTimer
  UI.start gcLiveBytesTimer
  UI.start cpuTimeGCTimer
  UI.start cpuTimeAppTimer
  UI.start threadsNumTimer

  on UI.disconnect window . const $ do
    -- The connection with the browser was dropped (probably user closed the tab),
    -- so timers should be stopped.
    UI.stop uiUpdateTimer
    UI.stop uiUpdateChartsTimer
    -- To restore current displayed state after DOM-rerendering.
    liftIO $ pageWasReload reloadFlag

  void $ UI.element pageBody

mkChartTimer
  :: ConnectedNodes
  -> Int
  -> History
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> DataName
  -> ChartId
  -> UI UI.Timer
mkChartTimer connectedNodes periodInMs history
             datasetIndices datasetTimestamps dataName chartId = do
  uiUpdateTimer <- UI.timer # set UI.interval periodInMs
  on UI.tick uiUpdateTimer . const $
    addAllPointsToChart connectedNodes history datasetIndices datasetTimestamps dataName chartId
  return uiUpdateTimer
