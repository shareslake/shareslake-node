{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.JS.Charts
  ( prepareChartsJS
  , addDatasetChartJS
  , addPointsChartJS
  , getDatasetsLengthChartJS
  , newTimeChartJS
  ) where

import           Control.Monad (forM_)
import           Data.String.QQ
import           Data.Text (Text)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.Update.Utils

prepareChartsJS :: UI ()
prepareChartsJS =
  UI.runFunction $ UI.ffi "window.charts = new Map();"

newTimeChartJS :: String -> UI ()
newTimeChartJS chartId = 
  UI.runFunction $ UI.ffi newTimeChartJS' chartId

newTimeChartJS' :: String
newTimeChartJS' = [s|
var ctx = document.getElementById(%1).getContext('2d');
var chart = new Chart(ctx, {
  type: 'line',
  data: {
    datasets: []
  },
  options: {
    responsive: true,
    scales: {
      xAxes: [{
        type: 'time',
        time: {
          displayFormats: {
            'millisecond': 'MM DD YYYY',
            'second': 'MM DD YYYY',
            'minute': 'MM DD YYYY',
            'hour': 'MM DD YYYY',
            'day': 'MM DD YYYY',
            'week': 'MM DD YYYY',
            'month': 'MM DD YYYY',
            'quarter': 'MM DD YYYY',
            'year': 'MM DD YYYY'
          }
        }
      }]
    }
  }
});
window.charts.set(%1, chart);
|]

{-
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
-}

addDatasetChartJS
  :: String
  -> Text
  -> String
  -> UI ()
addDatasetChartJS chartId nodeName color =
  UI.runFunction $ UI.ffi addDatasetChartJS' chartId nodeName color

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
window.charts.get(%1).resize();
|]

getDatasetsLengthChartJS :: String -> UI Int
getDatasetsLengthChartJS chartId =
  UI.callFunction $ UI.ffi "window.charts.get(%1).data.datasets.length;" chartId  

addPointsChartJS
  :: String
  -> Int
  -> [(POSIXTime, ValueH)]
  -> UI ()
addPointsChartJS chartId datasetIx points = do
  forM_ points $ \(ts, valueH) -> do
    let (tsFormatted :: String) = formatTime defaultTimeLocale "%F %T" $ s2utc ts
    UI.runFunction $ UI.ffi addPointsChartJS' chartId datasetIx tsFormatted (show valueH)

addPointsChartJS' :: String
addPointsChartJS' = [s|
window.charts.get(%1).data.datasets[%2].data.push({x: %3, y: %4});
window.charts.get(%1).update({duration: 0});
window.charts.get(%1).resize();
|]

-- resizeChartJS :: String
-- resizeChartJS = "window.charts.get(%1).resize();"
