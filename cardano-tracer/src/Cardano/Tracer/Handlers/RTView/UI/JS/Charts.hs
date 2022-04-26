{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.JS.Charts
  ( ChartTimeFormat (..)
  , ChartTimeUnit (..)
  , prepareChartsJS
  , addDatasetChartJS
  , addPointsChartJS
  , getDatasetsLengthChartJS
  , newTimeChartJS
  , setTimeFormatChartJS
  , setTimeUnitChartJS
  , resetZoomChartJS
  , changeColorsChartJS
  , ix2tf
  , ix2tu
  ) where

import           Data.List (intercalate)
import           Data.String.QQ
import           Data.Text (Text)
import           Data.Word (Word8)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Handlers.RTView.UI.Types

data ChartTimeFormat
  = TimeOnly
  | TimeAndDate
  | DateOnly

data ChartTimeUnit
  = Seconds
  | Minutes
  | Hours

ix2tf :: Index -> ChartTimeFormat
ix2tf (Index ix) =
  case ix of
    0 -> TimeOnly
    1 -> TimeAndDate
    2 -> DateOnly
    _ -> TimeOnly

ix2tu :: Index -> ChartTimeUnit
ix2tu (Index ix) =
  case ix of
    0 -> Seconds
    1 -> Minutes
    2 -> Hours
    _ -> Seconds

prepareChartsJS :: UI ()
prepareChartsJS =
  UI.runFunction $ UI.ffi "window.charts = new Map();"

newTimeChartJS
  :: ChartId
  -> String
  -> String
  -> UI ()
newTimeChartJS chartId chartName yValuesLabel =
  UI.runFunction $ UI.ffi newTimeChartJS' (show chartId) chartName yValuesLabel

newTimeChartJS' :: String
newTimeChartJS' = [s|
var ctx = document.getElementById(%1).getContext('2d');
var chart = new Chart(ctx, {
  type: 'line',
  data: {
    datasets: []
  },
  options: {
    animation: false,
    showLine: true,
    elements: {
      point: {
        radius: 2
      }
    },
    responsive: true,
    plugins: {
      title: {
        display: true,
        align: 'start',
        font: {
          size: 18
        },
        text: %2
      },
      zoom: {
        zoom: {
          drag: {
            enabled: true
          },
          mode: 'x'
        }
      }
    },
    transitions: {
      zoom: {
        animation: {
          duration: 0
        }
      }
    },
    scales: {
      x: {
        type: 'time',
        title: {
          display: true,
          text: 'Time in UTC'
        },
        time: {
          displayFormats: {
            second: 'MMM D YYYY HH:mm:ss',
            minute: 'MMM D YYYY HH:mm',
            hour:   'MMM D YYYY hh a',
          },
          unit: 'second'
        }
      },
      y: {
        title: {
          display: true,
          text: %3
        }
      }
    }
  }
});
window.charts.set(%1, chart);
|]

addDatasetChartJS
  :: ChartId
  -> Text
  -> Color
  -> UI ()
addDatasetChartJS chartId nodeName (Color color) =
  UI.runFunction $ UI.ffi addDatasetChartJS' (show chartId) nodeName color

addDatasetChartJS' :: String
addDatasetChartJS' = [s|
const newDataset = {
  label: %2,
  backgroundColor: %3,
  borderColor: %3,
  data: [],
  fill: false
};
window.charts.get(%1).data.datasets.push(newDataset);
window.charts.get(%1).update({duration: 0});
|]

getDatasetsLengthChartJS :: ChartId -> UI Word8
getDatasetsLengthChartJS chartId = do
  (l :: Int) <- UI.callFunction $ UI.ffi "window.charts.get(%1).data.datasets.length;" (show chartId)
  return $ fromIntegral l

addPointsChartJS
  :: ChartId
  -> Index
  -> [(POSIXTime, ValueH)]
  -> UI ()
addPointsChartJS chartId (Index datasetIx) points = do
  UI.runFunction $ UI.ffi pushToDataset
  UI.runFunction $ UI.ffi "window.charts.get(%1).update({duration: 0});" (show chartId)
 where
  pushToDataset =
    "window.charts.get('"
    <> (show chartId)
    <> "').data.datasets["
    <> show datasetIx
    <> "].data.push("
    <> pointsList
    <> ");"
  pointsList = intercalate ", " $ map mkPointObject points
  mkPointObject (ts, valueH) =
    "{x: '" <> show (s2utc ts) <> "', y: " <> show valueH <> "}"

setTimeFormatChartJS
  :: ChartId
  -> ChartTimeFormat
  -> UI ()
setTimeFormatChartJS chartId format =
  UI.runFunction $ UI.ffi setTimeFormatChartJS' (show chartId) formatSecond formatMinute formatHour
 where
  (formatSecond, formatMinute, formatHour) =
    case format of
      TimeOnly    -> (timeS,                timeM,                timeH)
      TimeAndDate -> (date <> " " <> timeS, date <> " " <> timeM, date <> " " <> timeH)
      DateOnly    -> (date,                 date,                 date)
  date  = "MMM D YYYY"
  timeS = "HH:mm:ss"
  timeM = "HH:mm"
  timeH = "hh a"

setTimeFormatChartJS' :: String
setTimeFormatChartJS' = [s|
window.charts.get(%1).options.scales.x.time.displayFormats.second = %2;
window.charts.get(%1).options.scales.x.time.displayFormats.minute = %3;
window.charts.get(%1).options.scales.x.time.displayFormats.hour = %4;
window.charts.get(%1).update({duration: 0});
|]

setTimeUnitChartJS
  :: ChartId
  -> ChartTimeUnit
  -> UI ()
setTimeUnitChartJS chartId Seconds = UI.runFunction $ UI.ffi setTimeUnitChartJS' (show chartId) "second"
setTimeUnitChartJS chartId Minutes = UI.runFunction $ UI.ffi setTimeUnitChartJS' (show chartId) "minute"
setTimeUnitChartJS chartId Hours   = UI.runFunction $ UI.ffi setTimeUnitChartJS' (show chartId) "hour"

setTimeUnitChartJS' :: String
setTimeUnitChartJS' = [s|
window.charts.get(%1).options.scales.x.time.unit = %2;
window.charts.get(%1).update({duration: 0});
|]

resetZoomChartJS :: ChartId -> UI ()
resetZoomChartJS chartId =
  UI.runFunction $ UI.ffi "window.charts.get(%1).resetZoom();" (show chartId)

changeColorsChartJS
  :: ChartId
  -> Color
  -> Color
  -> UI ()
changeColorsChartJS chartId (Color textColor) (Color gridColor) =
  UI.runFunction $ UI.ffi changeColorsChartJS' (show chartId) textColor gridColor

changeColorsChartJS' :: String
changeColorsChartJS' = [s|
window.charts.get(%1).options.scales.x.ticks.color = %2;
window.charts.get(%1).options.scales.x.title.color = %2;
window.charts.get(%1).options.scales.x.grid.color = %3;
window.charts.get(%1).options.scales.y.ticks.color = %2;
window.charts.get(%1).options.scales.y.title.color = %2;
window.charts.get(%1).options.scales.y.grid.color = %3;
window.charts.get(%1).options.plugins.title.color = %2;
window.charts.get(%1).options.plugins.legend.title.color = %2;
window.charts.get(%1).update({duration: 0});
|]
