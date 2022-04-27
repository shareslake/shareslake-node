{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Body
  ( mkPageBody
  ) where

import           Control.Monad (void)
import           Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.About
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkPageBody
  :: UI.Window
  -> Network
  -> UI Element
mkPageBody window networkConfig = do
  -- Resources charts.
  cpuChart          <- mkChart window CPUChart
  memoryChart       <- mkChart window MemoryChart
  gcMajorNumChart   <- mkChart window GCMajorNumChart
  gcMinorNumChart   <- mkChart window GCMinorNumChart
  gcLiveMemoryChart <- mkChart window GCLiveMemoryChart
  cpuTimeGCChart    <- mkChart window CPUTimeGCChart
  cpuTimeAppChart   <- mkChart window CPUTimeAppChart
  threadsNumChart   <- mkChart window ThreadsNumChart
  -- Blockchain charts.
  chainDensityChart <- mkChart window ChainDensityChart
  slotNumChart      <- mkChart window SlotNumChart
  blockNumChart     <- mkChart window BlockNumChart
  slotInEpochChart  <- mkChart window SlotInEpochChart
  epochChart        <- mkChart window EpochChart

  showHideChain     <- image "has-tooltip-multiline has-tooltip-right rt-view-show-hide-chart-group" showSVG
                             # set dataTooltip "Click to hide Chain Metrics"
                             # set dataState shownState
  showHideResources <- image "has-tooltip-multiline has-tooltip-right rt-view-show-hide-chart-group" showSVG
                             # set dataTooltip "Click to hide Resources Metrics"
                             # set dataState shownState

  body <-
    UI.getBody window #+
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
      ]

  on UI.click showHideChain . const $
    changeVisibilityForCharts window showHideChain "chain-charts" "Chain Metrics"
  on UI.click showHideResources . const $
    changeVisibilityForCharts window showHideResources "resources-charts" "Resources Metrics"

  Chart.prepareChartsJS

  Chart.newTimeChartJS CPUChart          "CPU usage"             "Percent"
  Chart.newTimeChartJS MemoryChart       "Memory usage"          "MB"
  Chart.newTimeChartJS GCMajorNumChart   "Number of major GCs"   ""
  Chart.newTimeChartJS GCMinorNumChart   "Number of minor GCs"   ""
  Chart.newTimeChartJS GCLiveMemoryChart "GC, live data in heap" "MB"
  Chart.newTimeChartJS CPUTimeGCChart    "CPU time used by GC"   "milliseconds"
  Chart.newTimeChartJS CPUTimeAppChart   "CPU time used by app"  "milliseconds"
  Chart.newTimeChartJS ThreadsNumChart   "Number of threads"     ""

  Chart.newTimeChartJS ChainDensityChart "Chain density"         "Percent"
  Chart.newTimeChartJS SlotNumChart      "Slot height"           ""
  Chart.newTimeChartJS BlockNumChart     "Block height"          ""
  Chart.newTimeChartJS SlotInEpochChart  "Slot in epoch"         ""
  Chart.newTimeChartJS EpochChart        "Epoch"                 ""

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
                 in "sockets " <> concat (intersperse ", " socks) <> "."
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
  -> ChartId
  -> UI Element
mkChart window chartId = do
  selectTimeFormat <-
    UI.select ## (show chartId <> show TimeFormatSelect) #+
      [ UI.option # set value "0"
                  # set text "Time only"
      , UI.option # set value "1"
                  # set text "Time and date"
      , UI.option # set value "2"
                  # set text "Date only"
      ]
  selectTimeUnit <-
    UI.select ## (show chartId <> show TimeUnitSelect) #+
      [ UI.option # set value "0"
                  # set text "Seconds"
      , UI.option # set value "1"
                  # set text "Minutes"
      , UI.option # set value "2"
                  # set text "Hours"
      ]
  resetZoom <- UI.button #. "button is-small is-info is-outlined"
                         # set text "Reset zoom"
  chart <-
    UI.div #. "rt-view-chart-container" #+
      [ UI.canvas ## (show chartId) #. "rt-view-chart-area" #+ []
      , UI.div #. "field is-grouped mt-3" #+
          [ UI.div #. "select is-link is-small mr-4" #+
              [ element selectTimeFormat
              ]
          , UI.div #. "select is-link is-small mr-4" #+
              [ element selectTimeUnit
              ]
          , element resetZoom
          ]
      ]

  on UI.selectionChange selectTimeFormat $ \optionIx -> do
    case optionIx of
      Just 0 -> Chart.setTimeFormatChartJS chartId Chart.TimeOnly
      Just 1 -> Chart.setTimeFormatChartJS chartId Chart.TimeAndDate
      Just 2 -> Chart.setTimeFormatChartJS chartId Chart.DateOnly
      _ -> return ()
    saveChartsSettings window

  on UI.selectionChange selectTimeUnit $ \optionIx -> do
    case optionIx of
      Just 0 -> Chart.setTimeUnitChartJS chartId Chart.Seconds
      Just 1 -> Chart.setTimeUnitChartJS chartId Chart.Minutes
      Just 2 -> Chart.setTimeUnitChartJS chartId Chart.Hours
      _ -> return ()
    saveChartsSettings window

  on UI.click resetZoom . const $ Chart.resetZoomChartJS chartId

  return chart

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
