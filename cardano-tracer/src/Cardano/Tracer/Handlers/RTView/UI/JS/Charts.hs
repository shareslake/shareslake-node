module Cardano.Tracer.Handlers.RTView.UI.JS.Charts
  ( prepareChartsJS
  -- Charts JS snippets.
  , memoryUsageChartJS
  , cpuUsageChartJS
  , cpuUsageChartJS2
  , diskUsageChartJS
  , networkUsageChartJS
  -- Charts updaters.
  , updateMemoryUsageChartJS
  , updateCPUUsageChartJS
  , addDatasetChartJS
  , getDatasetsLengthChartJS
  , addNewPointChartJS
  , addNewPointChartJS2

  , updateDiskUsageChartJS
  , updateNetworkUsageChartJS
  , resizeChartJS
  ) where

prepareChartsJS :: String
prepareChartsJS =
  "window.charts = new Map();"

memoryUsageChartJS :: String
memoryUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Memory | kernel',"
  , "      backgroundColor: '#FF8000',"
  , "      borderColor: '#FF8000',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'Memory | RTS',"
  , "      backgroundColor: '#DC143C',"
  , "      borderColor: '#DC143C',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'MB'"
  , "        },"
  , "        ticks: {"
  , "          min: 0"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

cpuUsageChartJS :: String
cpuUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: []"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'Percent'"
  , "        },"
  , "        ticks: {"
  , "          min: 0"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

cpuUsageChartJS2 :: String
cpuUsageChartJS2 = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    datasets: []"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      xAxes: [{"
  , "        type: 'time',"
  , "        time: {"
  , "          tooltipFormat: 'T'"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

diskUsageChartJS :: String
diskUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Disk | RD',"
  , "      backgroundColor: '#0080FF',"
  , "      borderColor: '#0080FF',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'Disk | WR',"
  , "      backgroundColor: '#D358F7',"
  , "      borderColor: '#D358F7',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'KB/s'"
  , "        },"
  , "        ticks: {"
  , "          min: 0"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

networkUsageChartJS :: String
networkUsageChartJS = concat
  [ "var ctx = document.getElementById(%1).getContext('2d');"
  , "var chart = new Chart(ctx, {"
  , "  type: 'line',"
  , "  data: {"
  , "    labels: [],"
  , "    datasets: [{"
  , "      label: 'Network | IN',"
  , "      backgroundColor: '#D7DF01',"
  , "      borderColor: '#D7DF01',"
  , "      data: [],"
  , "      fill: false"
  , "    },{"
  , "      label: 'Network | OUT',"
  , "      backgroundColor: '#00FF80',"
  , "      borderColor: '#00FF80',"
  , "      data: [],"
  , "      fill: false"
  , "    }]"
  , "  },"
  , "  options: {"
  , "    responsive: true,"
  , "    scales: {"
  , "      yAxes: [{"
  , "        display: true,"
  , "        scaleLabel: {"
  , "          display: true,"
  , "          labelString: 'KB/s'"
  , "        },"
  , "        ticks: {"
  , "          min: 0"
  , "        }"
  , "      }]"
  , "    }"
  , "  }"
  , "});"
  , "window.charts.set(%1, chart);"
  ]

-- Chart updaters.
-- Please note that after 900 data points (which are collected in every 30 minutes)
-- we remove outdated points. It allows to avoid too compressed, narrow charts.
-- This is a temporary solution, it will be improved in the future releases.

updateMemoryUsageChartJS
  , updateDiskUsageChartJS
  , updateNetworkUsageChartJS :: String
updateMemoryUsageChartJS  = updateDoubleDatasetChartJS
updateDiskUsageChartJS    = updateDoubleDatasetChartJS
updateNetworkUsageChartJS = updateDoubleDatasetChartJS

updateCPUUsageChartJS :: String
updateCPUUsageChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]

addDatasetChartJS :: String
addDatasetChartJS = concat
  [ "const newDataset = {"
  , "  label: %2,"
  , "  backgroundColor: %3,"
  , "  borderColor: %3,"
  , "  data: [],"
  , "  fill: false"
  , "};"
  , "window.charts.get(%1).data.datasets.push(newDataset);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]

getDatasetsLengthChartJS :: String
getDatasetsLengthChartJS = concat
  [ "window.charts.get(%1).data.datasets.length;"
  ]

addNewPointChartJS :: String
addNewPointChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "window.charts.get(%1).data.datasets[%3].data.push(%4);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]

addNewPointChartJS2 :: String
addNewPointChartJS2 = concat
  [ -- "window.charts.get(%1).data.labels.push(%2);"
    "window.charts.get(%1).data.datasets[%2].data.push({x: %3, y: %4});"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]

{-
updateSingleDatasetChartJS :: String
updateSingleDatasetChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "var len = window.charts.get(%1).data.labels.length;"
  , "if (len == 900) {"
  , "  window.charts.get(%1).data.datasets[0].data.splice(0, len);"
  , "  window.charts.get(%1).data.labels.splice(0, len);"
  , "}"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]
-}

updateDoubleDatasetChartJS :: String
updateDoubleDatasetChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "var len = window.charts.get(%1).data.labels.length;"
  , "if (len == 900) {"
  , "  window.charts.get(%1).data.datasets[0].data.splice(0, len);"
  , "  window.charts.get(%1).data.datasets[1].data.splice(0, len);"
  , "  window.charts.get(%1).data.labels.splice(0, len);"
  , "}"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).data.datasets[1].data.push(%4);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]

{-
updateThreeDatasetsChartJS :: String
updateThreeDatasetsChartJS = concat
  [ "window.charts.get(%1).data.labels.push(%2);"
  , "var len = window.charts.get(%1).data.labels.length;"
  , "if (len == 900) {"
  , "  window.charts.get(%1).data.datasets[0].data.splice(0, len);"
  , "  window.charts.get(%1).data.datasets[1].data.splice(0, len);"
  , "  window.charts.get(%1).data.datasets[2].data.splice(0, len);"
  , "  window.charts.get(%1).data.labels.splice(0, len);"
  , "}"
  , "window.charts.get(%1).data.datasets[0].data.push(%3);"
  , "window.charts.get(%1).data.datasets[1].data.push(%4);"
  , "window.charts.get(%1).data.datasets[2].data.push(%5);"
  , "window.charts.get(%1).update({duration: 0});"
  , "window.charts.get(%1).resize();"
  ]
-}

-- During changing of panes' size we have to explicitly recise charts.
resizeChartJS :: String
resizeChartJS = "window.charts.get(%1).resize();"
