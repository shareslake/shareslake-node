{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.Charts
  ( DatasetsTimestamps
  , initColors
  , initDatasetsIndices
  , initDatasetsTimestamps
  , getDatasetIx
  , addNodeDatasetsToCharts
  , addPointsToChart
  , getLatestDisplayedTS
  , saveLatestDisplayedTS
  , restoreChartsSettings
  , saveChartsSettings
  , changeChartsToLightTheme
  , changeChartsToDarkTheme
  ) where

-- | The module 'Cardano.Tracer.Handlers.RTView.UI.JS.Charts' contains the tools
--   for rendering/updating charts using Chart.JS library, via JS FFI.
--
--   This module contains common tools for charts' state. We need it to be able
--   to re-render their values after web-page reloading.

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (forM, forM_)
import           Control.Monad.Extra (whenJustM)
import           Data.Aeson
import qualified Data.Map.Strict as M
import           Data.Text (pack)
import           Data.Word (Word8)
import           Graphics.UI.Threepenny.Core
import           System.Directory
import           Text.Read (readMaybe)

import           Cardano.Tracer.Types (NodeId (..))
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Utils as JS
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils

chartsIds :: [ChartId]
chartsIds = [minBound .. maxBound]

initColors :: UI Colors
initColors = liftIO $ do
  q <- newTBQueueIO . fromIntegral $ length colors
  mapM_ (atomically . writeTBQueue q . Color) colors
  return q
 where
  -- | There are unique colors for each chart line corresponding to each connected node.
  --   To make chart lines visually distinct, the colors in this list are contrast enough.
  --   It is assumed that the number of colors in this list is enough.
  colors =
    [ "#ff0000", "#66ff33", "#0066ff", "#ff00ff", "#cc0066", "#00ccff", "#ff9933", "#cc9900"
    , "#33cc33", "#0099ff"
    ]

getNewColor :: Colors -> UI Color
getNewColor q =
  (liftIO . atomically $ tryReadTBQueue q) >>= \case
    Just color -> return color
    Nothing    -> return defaultColor
 where
  defaultColor = Color "#cccc00"

initDatasetsIndices :: UI DatasetsIndices
initDatasetsIndices = liftIO . newTVarIO $ M.empty

saveDatasetIx
  :: DatasetsIndices
  -> NodeId
  -> Index
  -> UI ()
saveDatasetIx indices nodeId ix = liftIO . atomically $
  modifyTVar' indices $ \currentIndices ->
    case M.lookup nodeId currentIndices of
      Nothing -> M.insert nodeId ix currentIndices
      Just _  -> M.adjust (const ix) nodeId currentIndices

getDatasetIx
  :: DatasetsIndices
  -> NodeId
  -> UI (Maybe Index)
getDatasetIx indices nodeId = liftIO $
  M.lookup nodeId <$> readTVarIO indices

addNodeDatasetsToCharts
  :: Window
  -> NodeId
  -> Colors
  -> DatasetsIndices
  -> DisplayedElements
  -> UI ()
addNodeDatasetsToCharts window nodeId@(NodeId anId) colors datasetIndices displayedElements = do
  colorForNode@(Color code) <- getNewColor colors
  forM_ chartsIds $ \chartId -> do
    newIx <- Chart.getDatasetsLengthChartJS chartId
    nodeName <- liftIO $ getDisplayedValue displayedElements nodeId (anId <> "__node-name")
    Chart.addDatasetChartJS chartId (maybe anId id nodeName) colorForNode
    saveDatasetIx datasetIndices nodeId (Index newIx)
  -- Change color label for node name as well.
  findAndSet (set style [("color", code)]) window (anId <> "__node-chart-label")

initDatasetsTimestamps :: UI DatasetsTimestamps
initDatasetsTimestamps = liftIO . newTVarIO $ M.empty

saveLatestDisplayedTS
  :: DatasetsTimestamps
  -> NodeId
  -> DataName
  -> POSIXTime
  -> UI ()
saveLatestDisplayedTS tss nodeId dataName ts = liftIO . atomically $
  modifyTVar' tss $ \currentTimestamps ->
    case M.lookup nodeId currentTimestamps of
      Nothing ->
        -- There is no latest timestamps for charts for this node yet.
        let newTSForNode = M.singleton dataName ts
        in M.insert nodeId newTSForNode currentTimestamps
      Just tssForNode ->
        let newTSForNode =
              case M.lookup dataName tssForNode of
                Nothing ->
                  -- There is no latest timestamps for this dataName yet.
                  M.insert dataName ts tssForNode
                Just _ ->
                  M.adjust (const ts) dataName tssForNode
        in M.adjust (const newTSForNode) nodeId currentTimestamps

getLatestDisplayedTS
  :: DatasetsTimestamps
  -> NodeId
  -> DataName
  -> UI (Maybe POSIXTime)
getLatestDisplayedTS tss nodeId dataName = liftIO $
  (M.lookup nodeId <$> readTVarIO tss) >>= \case
    Nothing         -> return Nothing
    Just tssForNode -> return $ M.lookup dataName tssForNode

addPointsToChart
  :: ChartId
  -> NodeId
  -> DatasetsIndices
  -> [(POSIXTime, ValueH)]
  -> UI ()
addPointsToChart _ _ _ [] = return ()
addPointsToChart chartId nodeId datasetIndices points =
  whenJustM (getDatasetIx datasetIndices nodeId) $ \datasetIx ->
    Chart.addPointsChartJS chartId datasetIx points

restoreChartsSettings :: UI ()
restoreChartsSettings = readSavedChartsSettings >>= setCharts
 where
  setCharts settings =
    forM_ settings $ \(chartId, ChartSettings tf tu) -> do
      JS.selectOption (show chartId <> show TimeFormatSelect) tf
      JS.selectOption (show chartId <> show TimeUnitSelect)   tu
      Chart.setTimeFormatChartJS chartId $ Chart.ix2tf tf
      Chart.setTimeUnitChartJS   chartId $ Chart.ix2tu tu

saveChartsSettings :: Window -> UI ()
saveChartsSettings window = do
  settings <-
    forM chartsIds $ \chartId -> do
      selectedTF <- getOptionIndex $ show chartId <> show TimeFormatSelect
      selectedTU <- getOptionIndex $ show chartId <> show TimeUnitSelect
      return (chartId, ChartSettings (Index selectedTF) (Index selectedTU))
  liftIO . ignore $ do
    pathToChartsConfig <- getPathToChartsConfig
    encodeFile pathToChartsConfig settings
 where
  getOptionIndex selectId =
    (readMaybe <$> findAndGetValue window (pack selectId)) >>= \case
      Just (ix :: Word8) -> return ix
      Nothing -> return 0

readSavedChartsSettings :: UI ChartsSettings
readSavedChartsSettings = liftIO $
  try_ (decodeFileStrict' =<< getPathToChartsConfig) >>= \case
    Right (Just (settings :: ChartsSettings)) -> return settings
    _ -> return defaultSettings
 where
  defaultSettings =
    [ (chartId, ChartSettings (Index 0) (Index 0))
    | chartId <- chartsIds
    ]

getPathToChartsConfig :: IO FilePath
getPathToChartsConfig = getXdgDirectory XdgConfig "rt-view-charts-config"

changeChartsToLightTheme :: UI ()
changeChartsToLightTheme =
  forM_ chartsIds $ \chartId ->
    Chart.changeColorsChartJS chartId (Color chartTextDark) (Color chartGridDark)

changeChartsToDarkTheme :: UI ()
changeChartsToDarkTheme =
  forM_ chartsIds $ \chartId ->
    Chart.changeColorsChartJS chartId (Color chartTextLight) (Color chartGridLight)
