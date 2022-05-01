{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Body
  ( mkPageBody
  ) where

import           Control.Monad (void, unless, when)
import           Control.Monad.Extra (whenM, whenJustM)
import           Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Text.Read (readMaybe)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.About
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

mkPageBody
  :: UI.Window
  -> Network
  -> ConnectedNodes
  -> ResourcesHistory
  -> BlockchainHistory
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> UI Element
mkPageBody window networkConfig connectedNodes
           (ResHistory rHistory) (ChainHistory cHistory)
           datasetIndices datasetTimestamps = do
  -- Resources charts.
  cpuTimer          <-
    mkChartTimer connectedNodes rHistory datasetIndices datasetTimestamps CPUData          CPUChart
  memoryTimer       <-
    mkChartTimer connectedNodes rHistory datasetIndices datasetTimestamps MemoryData       MemoryChart
  gcMajorNumTimer   <-
    mkChartTimer connectedNodes rHistory datasetIndices datasetTimestamps GCMajorNumData   GCMajorNumChart
  gcMinorNumTimer   <-
    mkChartTimer connectedNodes rHistory datasetIndices datasetTimestamps GCMinorNumData   GCMinorNumChart
  gcLiveMemoryTimer <-
    mkChartTimer connectedNodes rHistory datasetIndices datasetTimestamps GCLiveMemoryData GCLiveMemoryChart
  cpuTimeGCTimer    <-
    mkChartTimer connectedNodes rHistory datasetIndices datasetTimestamps CPUTimeGCData    CPUTimeGCChart
  cpuTimeAppTimer   <-
    mkChartTimer connectedNodes rHistory datasetIndices datasetTimestamps CPUTimeAppData   CPUTimeAppChart
  threadsNumTimer   <-
    mkChartTimer connectedNodes rHistory datasetIndices datasetTimestamps ThreadsNumData   ThreadsNumChart

  cpuChart          <- mkChart window cpuTimer          CPUChart          "CPU usage"
  memoryChart       <- mkChart window memoryTimer       MemoryChart       "Memory usage"
  gcMajorNumChart   <- mkChart window gcMajorNumTimer   GCMajorNumChart   "Number of major GCs"
  gcMinorNumChart   <- mkChart window gcMinorNumTimer   GCMinorNumChart   "Number of minor GCs"
  gcLiveMemoryChart <- mkChart window gcLiveMemoryTimer GCLiveMemoryChart "GC, live data in heap"
  cpuTimeGCChart    <- mkChart window cpuTimeGCTimer    CPUTimeGCChart    "CPU time used by GC"
  cpuTimeAppChart   <- mkChart window cpuTimeAppTimer   CPUTimeAppChart   "CPU time used by app"
  threadsNumChart   <- mkChart window threadsNumTimer   ThreadsNumChart   "Number of threads"

  -- Blockchain charts.
  chainDensityTimer <-
    mkChartTimer connectedNodes cHistory datasetIndices datasetTimestamps ChainDensityData ChainDensityChart
  slotNumTimer      <-
    mkChartTimer connectedNodes cHistory datasetIndices datasetTimestamps SlotNumData      SlotNumChart
  blockNumTimer     <-
    mkChartTimer connectedNodes cHistory datasetIndices datasetTimestamps BlockNumData     BlockNumChart
  slotInEpochTimer  <-
    mkChartTimer connectedNodes cHistory datasetIndices datasetTimestamps SlotInEpochData  SlotInEpochChart
  epochTimer        <-
    mkChartTimer connectedNodes cHistory datasetIndices datasetTimestamps EpochData        EpochChart

  chainDensityChart <- mkChart window chainDensityTimer ChainDensityChart "Chain density"
  slotNumChart      <- mkChart window slotNumTimer      SlotNumChart      "Slot height"
  blockNumChart     <- mkChart window blockNumTimer     BlockNumChart     "Block height"
  slotInEpochChart  <- mkChart window slotInEpochTimer  SlotInEpochChart  "Slot in epoch"
  epochChart        <- mkChart window epochTimer        EpochChart        "Epoch"

  -- Visibility of charts gropus.
  showHideChain     <- image "has-tooltip-multiline has-tooltip-right rt-view-show-hide-chart-group" showSVG
                             # set dataTooltip "Click to hide Chain Metrics"
                             # set dataState shownState
  showHideResources <- image "has-tooltip-multiline has-tooltip-right rt-view-show-hide-chart-group" showSVG
                             # set dataTooltip "Click to hide Resources Metrics"
                             # set dataState shownState

  body <-
    UI.getBody window #+
      [ UI.div ## "wrapper" #+
          [ UI.div ## "preloader" #. "pageloader is-active" #+
              [ UI.span #. "title" # set text "Just a second..."
              ]
          , topNavigation window
          , UI.div ## "no-nodes" #. "container is-max-widescreen has-text-centered" #+
              [ image "rt-view-no-nodes-icon" noNodesSVG ## "no-nodes-icon"
              , UI.p ## "no-nodes-message" #. "rt-view-no-nodes-message" #+
                  [ string "There are no connected nodes. Yet."
                  ]
              ]
          , noNodesInfo networkConfig
          , UI.mkElement "section" #. "section" #+
              [ UI.div ## "main-table-container"
                       #. "table-container"
                       # hideIt #+
                  [ UI.table ## "main-table" #. "table rt-view-main-table" #+
                      [ UI.mkElement "thead" #+
                          [ UI.tr ## "node-name-row" #+
                              [ UI.th #. "rt-view-main-table-description"
                                      #+ [UI.span # set html "&nbsp;"]
                              ]
                          ]
                      , UI.mkElement "tbody" #+
                          [ UI.tr ## "node-version-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" versionSVG
                                         , string "Version"
                                         ]
                              ]
                          , UI.tr ## "node-protocol-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" protocolSVG
                                         , string "Protocol"
                                         ]
                              ]
                          , UI.tr ## "node-commit-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" commitSVG
                                         , string "Commit"
                                         ]
                              ]
                          , UI.tr ## "node-system-start-time-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" systemStartSVG
                                         , string "Blockchain start"
                                         ]
                              ]
                          , UI.tr ## "node-start-time-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" startSVG
                                         , string "Node start"
                                         ]
                              ]
                          , UI.tr ## "node-uptime-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" uptimeSVG
                                         , string "Uptime"
                                         ]
                              ]
                          , UI.tr ## "node-logs-row" #+
                              [ UI.td #+ [ image "rt-view-overview-icon" logsSVG
                                         , string "Logs"
                                         ]
                              ]
                          --, UI.tr ## "node-peers-row" #+
                          --    [ UI.td #+ [ image "rt-view-overview-icon" peersSVG
                          --               , string "Peers"
                          --               ]
                          --    ]
                          --, UI.tr ## "node-errors-row" #+
                          --    [ UI.td #+ [ image "rt-view-overview-icon" errorsSVG
                          --               , string "Errors"
                          --               ]
                          --    ]
                          ]
                      ]
                  ]
              ]
          , UI.div ## "main-charts-container"
                   #. "container is-fluid rt-view-charts-container"
                   # hideIt #+
              [ UI.p #+
                  [ UI.span #. "rt-view-chart-group-title" # set text "Chain Metrics"
                  , element showHideChain
                  ]
              , UI.div ## "chain-charts" #. "columns" #+
                  [ UI.div #. "column" #+
                      [ element chainDensityChart
                      , element epochChart
                      , element blockNumChart
                      ]
                  , UI.div #. "column" #+
                      [ element slotInEpochChart
                      , element slotNumChart
                      ]
                  ]
              , UI.p #+
                  [ UI.span #. "rt-view-chart-group-title" # set text "Resources Metrics"
                  , element showHideResources
                  ]
              , UI.div ## "resources-charts" #. "columns" #+
                  [ UI.div #. "column" #+
                      [ element cpuChart
                      , element gcMajorNumChart
                      , element gcLiveMemoryChart
                      , element cpuTimeGCChart
                      ]
                  , UI.div #. "column" #+
                      [ element memoryChart
                      , element gcMinorNumChart
                      , element threadsNumChart
                      , element cpuTimeAppChart
                      ]
                  ]
              ]
          , footer
          ]
      ]

  on UI.click showHideChain . const $
    changeVisibilityForCharts window showHideChain "chain-charts" "Chain Metrics"
  on UI.click showHideResources . const $
    changeVisibilityForCharts window showHideResources "resources-charts" "Resources Metrics"

  Chart.prepareChartsJS

  Chart.newTimeChartJS CPUChart          "Percent"
  Chart.newTimeChartJS MemoryChart       "MB"
  Chart.newTimeChartJS GCMajorNumChart   ""
  Chart.newTimeChartJS GCMinorNumChart   ""
  Chart.newTimeChartJS GCLiveMemoryChart "MB"
  Chart.newTimeChartJS CPUTimeGCChart    "milliseconds"
  Chart.newTimeChartJS CPUTimeAppChart   "milliseconds"
  Chart.newTimeChartJS ThreadsNumChart   ""

  Chart.newTimeChartJS ChainDensityChart "Percent"
  Chart.newTimeChartJS SlotNumChart      ""
  Chart.newTimeChartJS BlockNumChart     ""
  Chart.newTimeChartJS SlotInEpochChart  ""
  Chart.newTimeChartJS EpochChart        ""

  UI.start cpuTimer
  UI.start memoryTimer
  UI.start gcMajorNumTimer
  UI.start gcMinorNumTimer
  UI.start gcLiveMemoryTimer
  UI.start cpuTimeGCTimer
  UI.start cpuTimeAppTimer
  UI.start threadsNumTimer

  on UI.disconnect window . const $ do
    UI.stop cpuTimer
    UI.stop memoryTimer
    UI.stop gcMajorNumTimer
    UI.stop gcMinorNumTimer
    UI.stop gcLiveMemoryTimer
    UI.stop cpuTimeGCTimer
    UI.stop cpuTimeAppTimer
    UI.stop threadsNumTimer

  return body

topNavigation :: UI.Window -> UI Element
topNavigation window = do
  info <- mkAboutInfo
  infoIcon <- image "has-tooltip-multiline has-tooltip-bottom rt-view-info-icon" rtViewInfoSVG
                    ## "info-icon"
                    # set dataTooltip "RTView info"
  on UI.click infoIcon . const $ element info #. "modal is-active"

  --closeNotifications <- UI.button #. "modal-close is-large" #+ []
  --notifications <- mkOwnInfo closeNotifications
  --notifyIcon <- image "has-tooltip-multiline has-tooltip-bottom rt-view-notify-icon" rtViewNotifyLightSVG
  --                    ## "notify-icon"
  --                    # set dataTooltip "RTView notifications"
  --registerClicksForModal notifications notifyIcon closeNotifications

  themeIcon <- image "has-tooltip-multiline has-tooltip-bottom rt-view-theme-icon" rtViewThemeToLightSVG
                     ## "theme-icon"
                     # set dataTooltip "Switch to light theme"
  on UI.click themeIcon . const $ switchTheme window

  UI.div ## "top-bar" #. "navbar rt-view-top-bar" #+
    [ element info
    -- , element notifications
    , UI.div #. "navbar-brand" #+
        [ UI.div #. "navbar-item" #+
            [ image "rt-view-cardano-logo" cardanoLogoSVG ## "cardano-logo"
            , UI.span ## "name" #. "rt-view-name" # set text "Node Real-time View"
            ]
        ]
    , UI.div #. "navbar-menu" #+
        [ UI.div #. "navbar-start" #+ []
        , UI.div #. "navbar-end" #+
            [ -- UI.div #. "navbar-item" #+ [element notifyIcon]
              UI.div #. "navbar-item" #+ [element infoIcon]
            , UI.div #. "navbar-item" #+ [element themeIcon]
            ]
        ]
    ]

footer :: UI Element
footer =
  UI.mkElement "footer" #. "footer rt-view-footer" #+
    [ UI.div #. "columns" #+
        [ UI.div #. "column" #+
            [ string "© IOHK 2015—2022"
            ]
        , UI.div #. "column has-text-right" #+
            [ UI.anchor # set UI.href "https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/README.md"
                        # set UI.target "_blank" #+
                [ image "has-tooltip-multiline has-tooltip-left rt-view-footer-github" githubSVG
                        # set dataTooltip "Browse our GitHub repository"
                ]
            ]
        ]
    ]

-- | If the user doesn't see connected nodes - possible reason of it is
--   misconfiguration of 'cardano-tracer' and/or 'cardano-node'.
--   So we have to show basic explanation.
noNodesInfo :: Network -> UI Element
noNodesInfo networkConfig = do
  closeIt <- UI.button #. "delete"
  infoNote <-
    UI.div ## "no-nodes-info"
           #. "container notification is-link rt-view-no-nodes-info" #+
      [ element closeIt
      , UI.p #. "mb-4 is-size-4" # set text "«Hey, where are my nodes?»"
      , UI.p #+
          [ string "If you are sure that your nodes should already be connected, "
          , string "please check your configuration files."
          ]
      , UI.p #+
          [ string "For more details, please read "
          , UI.anchor # set UI.href "https://github.com/input-output-hk/cardano-node/blob/master/cardano-tracer/docs/cardano-tracer.md#configuration"
                      # set text "our documentation"
                      # set UI.target "_blank"
          , image "rt-view-href-icon" externalLinkWhiteSVG
          , string "."
          ]
      , UI.p #. "mt-4" #+
          [ UI.span # set UI.html ("Currently, your <code>cardano-tracer</code> is configured as a " <> mode)
          , UI.span # set UI.html (", so it " <> whatItDoes)
          ]
      , UI.p #. "mt-4" #+
          [ UI.span # set UI.html ("Correspondingly, your " <> nodeConfigNotice)
          ]
      ]
  on UI.click closeIt . const $ element infoNote # hideIt
  return infoNote
 where
  (mode, whatItDoes, nodeConfigNotice) =
    case networkConfig of
      AcceptAt (LocalSocket p) ->
        ( "server"
        , "accepts connections from your nodes via the local socket <code>" <> p <> "</code>."
        , "nodes should be configured as clients: make sure <code>TraceOptionForwarder.mode</code>"
          <> " is <code>Initiator</code>, also check <code>TraceOptionForwarder.address</code> path."
        )
      ConnectTo addrs ->
        let manySocks = NE.length addrs > 1 in
        ( "client"
        , "connects to your "
          <> (if manySocks then "nodes" else "node")
          <> " via the local "
          <> (if manySocks
               then
                 let socks = map (\(LocalSocket p) -> "<code>" <> p <> "</code>") $ NE.toList addrs
                 in "sockets " <> intercalate ", " socks <> "."
               else
                 "socket <code>" <> let LocalSocket p = NE.head addrs in p <> "</code>.")
        , (if manySocks then "nodes" else "node")
          <> " should be configured as "
          <> (if manySocks then "servers" else "a server")
          <> ": make sure <code>TraceOptionForwarder.mode</code>"
          <> " is <code>Responder</code>, also check <code>TraceOptionForwarder.address</code> path."
        )

mkChart
  :: UI.Window
  -> UI.Timer
  -> ChartId
  -> String
  -> UI Element
mkChart window chartUpdateTimer chartId chartName = do
  selectTimeRange <-
    UI.select ## (show chartId <> show TimeRangeSelect) #+
      -- Values are ranges in seconds.
      [ UI.option # set value "0"     # set text "All time"
      , UI.option # set value "300"   # set text "Last 5 minutes"
      , UI.option # set value "900"   # set text "Last 15 minutes"
      , UI.option # set value "1800"  # set text "Last 30 minutes"
      , UI.option # set value "3600"  # set text "Last 1 hour"
      , UI.option # set value "10800" # set text "Last 3 hours"
      , UI.option # set value "21600" # set text "Last 6 hours"
      ]
  selectUpdatePeriod <-
    UI.select ## (show chartId <> show UpdatePeriodSelect) #+
      -- Values are periods in seconds.
      [ UI.option # set value "0"    # set text "Off"
      , UI.option # set value "15"   # set text "15 seconds"
      , UI.option # set value "30"   # set text "30 seconds"
      , UI.option # set value "60"   # set text "1 minute"
      , UI.option # set value "300"  # set text "5 minutes"
      , UI.option # set value "900"  # set text "15 minutes"
      , UI.option # set value "1800" # set text "30 minutes"
      , UI.option # set value "3600" # set text "1 hour"
      ]

  on UI.selectionChange selectTimeRange . const $
    whenJustM (readMaybe <$> get value selectTimeRange) $ \(rangeInSec :: Int) -> do
      Chart.setTimeRange chartId rangeInSec
      when (rangeInSec == 0) $ Chart.resetZoomChartJS chartId
      saveChartsSettings window

  on UI.selectionChange selectUpdatePeriod . const $
    whenJustM (readMaybe <$> get value selectUpdatePeriod) $ \(periodInSec :: Int) -> do
      whenM (get UI.running chartUpdateTimer) $ UI.stop chartUpdateTimer
      unless (periodInSec == 0) $ do
        void $ return chartUpdateTimer # set UI.interval (periodInSec * 1000)
        UI.start chartUpdateTimer
      saveChartsSettings window

  UI.div #. "rt-view-chart-container" #+
    [ UI.div #. "columns" #+
        [ UI.div #. "column mt-1" #+
            [ UI.span #. "rt-view-chart-name" # set text chartName
            ]
        , UI.div #. "column has-text-right" #+
            [ UI.div #. "field is-grouped mt-3" #+
                [ image "has-tooltip-multiline has-tooltip-top rt-view-chart-icon" timeRangeSVG
                        # set dataTooltip "Select time range"
                , UI.div #. "select is-link is-small mr-4" #+ [element selectTimeRange]
                , image "has-tooltip-multiline has-tooltip-top rt-view-chart-icon" refreshSVG
                        # set dataTooltip "Select update period"
                , UI.div #. "select is-link is-small" #+ [element selectUpdatePeriod]
                ]
            ]
        ]
    , UI.canvas ## show chartId #. "rt-view-chart-area" #+ []
    ]

shownState, hiddenState :: String
shownState  = "shown"
hiddenState = "hidden"

changeVisibilityForCharts
  :: UI.Window
  -> Element
  -> Text
  -> String
  -> UI ()
changeVisibilityForCharts window showHideIcon areaId areaName = do
  state <- get dataState showHideIcon
  let haveToHide = state == shownState
  if haveToHide
    then do
      findAndHide window areaId
      void $ element showHideIcon # set html        hideSVG
                                  # set dataState   hiddenState
                                  # set dataTooltip ("Click to show " <> areaName)
    else do
      findAndSet showFlex window areaId
      void $ element showHideIcon # set html        showSVG
                                  # set dataState   shownState
                                  # set dataTooltip ("Click to hide " <> areaName)

mkChartTimer
  :: ConnectedNodes
  -> History
  -> DatasetsIndices
  -> DatasetsTimestamps
  -> DataName
  -> ChartId
  -> UI UI.Timer
mkChartTimer connectedNodes history datasetIndices datasetTimestamps dataName chartId = do
  uiUpdateTimer <- UI.timer # set UI.interval defaultUpdatePeriodInMs
  on UI.tick uiUpdateTimer . const $
    addAllPointsToChart connectedNodes history datasetIndices datasetTimestamps dataName chartId
  return uiUpdateTimer
 where
  defaultUpdatePeriodInMs = 15 * 1000
