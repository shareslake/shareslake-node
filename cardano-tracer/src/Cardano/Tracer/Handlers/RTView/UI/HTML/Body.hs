{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.HTML.Body
  ( mkPageBody
  ) where

import           Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.HTML.About
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart
import           Cardano.Tracer.Handlers.RTView.UI.Theme
import           Cardano.Tracer.Handlers.RTView.UI.Utils

mkPageBody
  :: UI.Window
  -> Network
  -> UI Element
mkPageBody window networkConfig = do
  cpuChart    <- mkChart "cpu-chart"
  memoryChart <- mkChart "memory-chart"

  body <-
    UI.getBody window #+
      [ UI.div ## "preloader" #. "pageloader is-active" #+
          [ UI.span #. "title" # set text "Just a second..."
          ]
      , topNavigation window
      , UI.div ## "no-nodes" #. "container is-max-widescreen has-text-centered" #+
          [ image "rt-view-no-nodes-icon" noNodesLightSVG ## "no-nodes-icon"
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
                          [ UI.td #. "rt-view-table-description-td"
                                  #+ [ image "rt-view-overview-icon" versionSVG
                                     , string "Version"
                                     ]
                          ]
                      , UI.tr ## "node-protocol-row" #+
                          [ UI.td #. "rt-view-table-description-td"
                                  #+ [ image "rt-view-overview-icon" protocolSVG
                                     , string "Protocol"
                                     ]
                          ]
                      , UI.tr ## "node-commit-row" #+
                          [ UI.td #. "rt-view-table-description-td"
                                  #+ [ image "rt-view-overview-icon" commitSVG
                                     , string "Commit"
                                     ]
                          ]
                      , UI.tr ## "node-start-time-row" #+
                          [ UI.td #. "rt-view-table-description-td"
                                  #+ [ image "rt-view-overview-icon" startSVG
                                     , string "Node start"
                                     ]
                          ]
                      , UI.tr ## "node-system-start-time-row" #+
                          [ UI.td #. "rt-view-table-description-td"
                                  #+ [ image "rt-view-overview-icon" systemStartSVG
                                     , string "System start"
                                     ]
                          ]
                      , UI.tr ## "node-uptime-row" #+
                          [ UI.td #. "rt-view-table-description-td"
                                  #+ [ image "rt-view-overview-icon" uptimeSVG
                                     , string "Uptime"
                                     ]
                          ]
                      , UI.tr ## "node-logs-row" #+
                          [ UI.td #. "rt-view-table-description-td"
                                  #+ [ image "rt-view-overview-icon" logsSVG
                                     , string "Logs"
                                     ]
                          ]
                      --, UI.tr ## "node-peers-row" #+
                      --    [ UI.td #. "rt-view-table-description-td"
                      --            #+ [ image "rt-view-overview-icon" peersSVG
                      --               , string "Peers"
                      --               ]
                      --    ]
                      , UI.tr ## "node-chain-row" #+
                          [ UI.td #. "rt-view-table-description-td"
                                  #+ [ image "rt-view-overview-icon" chainSVG
                                     , string "Chain"
                                     ]
                          ]
                      --, UI.tr ## "node-errors-row" #+
                      --    [ UI.td #. "rt-view-table-description-td"
                      --            #+ [ image "rt-view-overview-icon" errorsSVG
                      --               , string "Errors"
                      --               ]
                      --    ]
                      ]
                  ]
              ]
          ]
      , UI.div ## "main-charts-container"
               #. "container rt-view-charts-container"
               # hideIt #+
          [ UI.div #. "columns" #+
              [ UI.div #. "column" #+
                  [ element cpuChart
                  ]
              , UI.div #. "column" #+
                  [ element memoryChart
                  ]
              ]
          ]
      ]

  Chart.prepareChartsJS

  Chart.newTimeChartJS "cpu-chart"    "CPU Usage"    "Percent"
  Chart.newTimeChartJS "memory-chart" "Memory Usage" "MB"

  return body

topNavigation :: UI.Window -> UI Element
topNavigation window = do
  info <- mkAboutInfo
  infoIcon <- image "has-tooltip-multiline has-tooltip-bottom rt-view-info-icon" rtViewInfoLightSVG
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

  UI.div ## "top-bar" #. "navbar rt-view-top-bar-dark" #+
    [ element info
    -- , element notifications
    , UI.div #. "navbar-brand" #+
        [ UI.div #. "navbar-item" #+
            [ image "rt-view-cardano-logo" cardanoLogoLightSVG ## "cardano-logo"
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
  :: String
  -> UI Element
mkChart chartId = do
  selectTimeFormat <-
    UI.select ## (chartId <> "-time-format") #+
      [ UI.option # set value "t"
                  # set text "Time only"
      , UI.option # set value "td"
                  # set text "Time and date"
      , UI.option # set value "d"
                  # set text "Date only"
      ]
  selectTimeUnit <-
    UI.select ## (chartId <> "-time-unit") #+
      [ UI.option # set value "ss"
                  # set text "Seconds"
      , UI.option # set value "mm"
                  # set text "Minutes"
      , UI.option # set value "hh"
                  # set text "Hours"
      ]
  resetZoom <- UI.button #. "button is-small is-info is-outlined"
                         # set text "Reset zoom"
  chart <-
    UI.div #. "rt-view-chart-container" #+
      [ UI.canvas ## chartId #. "rt-view-chart-area" #+ []
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

  on UI.selectionChange selectTimeFormat $ \optionIx ->
    case optionIx of
      Just 0 -> Chart.setTimeFormatChartJS chartId Chart.TimeOnly
      Just 1 -> Chart.setTimeFormatChartJS chartId Chart.TimeAndDate
      Just 2 -> Chart.setTimeFormatChartJS chartId Chart.DateOnly
      _ -> return ()

  on UI.selectionChange selectTimeUnit $ \optionIx ->
    case optionIx of
      Just 0 -> Chart.setTimeUnitChartJS chartId Chart.Seconds
      Just 1 -> Chart.setTimeUnitChartJS chartId Chart.Minutes
      Just 2 -> Chart.setTimeUnitChartJS chartId Chart.Hours
      _ -> return ()

  on UI.click resetZoom . const $ Chart.resetZoomChartJS chartId

  return chart
