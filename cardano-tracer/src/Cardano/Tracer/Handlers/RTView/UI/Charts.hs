{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.UI.Charts
  ( Color
  , Colors
  , DatasetsIndices
  , DatasetsTimestamps
  , initColors
  , initDatasetsIndices
  , initDatasetsTimestamps
  , getDatasetIx
  , addNodeDatasetsToCharts
  , addPointsToChart
  , getLatestDisplayedTS
  , saveLatestDisplayedTS
  ) where

-- | The module 'Cardano.Tracer.Handlers.RTView.UI.JS.Charts' contains the tools
--   for rendering/updating charts using Chart.JS library, via JS FFI.
--
--   This module contains common tools for charts' state. We need it to be able
--   to re-render their values after web-page reloading.

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue
import           Control.Concurrent.STM.TVar
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJustM)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Types (NodeId (..))

import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Historical
import qualified Cardano.Tracer.Handlers.RTView.UI.JS.Charts as Chart

type Color = String
type Colors = TBQueue Color

initColors :: UI Colors
initColors = liftIO $ do
  q <- newTBQueueIO . fromIntegral $ length colors
  mapM_ (atomically . writeTBQueue q) colors
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
    Nothing    -> return "#cccc00"

-- | After the node is connected, we have to add a new dataset to all historical charts.
--   The metrics received from this node will be added in these datasets.
--   Since each dataset has its index, we need a map 'NodeId -> ix',
--   where 'ix' is an index of a dataset in _each_ chart.
type DatasetsIndices = TVar (Map NodeId Int)

initDatasetsIndices :: UI DatasetsIndices
initDatasetsIndices = liftIO . newTVarIO $ M.empty

saveDatasetIx
  :: DatasetsIndices
  -> NodeId
  -> Int
  -> UI ()
saveDatasetIx indices nodeId ix = liftIO . atomically $
  modifyTVar' indices $ \currentIndices ->
    case M.lookup nodeId currentIndices of
      Nothing -> M.insert nodeId ix currentIndices
      Just _  -> M.adjust (const ix) nodeId currentIndices

getDatasetIx
  :: DatasetsIndices
  -> NodeId
  -> UI (Maybe Int)
getDatasetIx indices nodeId = liftIO $
  M.lookup nodeId <$> readTVarIO indices

-- | ...
addNodeDatasetsToCharts
  :: NodeId
  -> Colors
  -> DatasetsIndices
  -> DisplayedElements
  -> UI ()
addNodeDatasetsToCharts nodeId@(NodeId anId) colors datasetIndices displayedElements =
  forM_ chartsIds $ \chartId -> do
    (newIx :: Int) <- UI.callFunction $ UI.ffi Chart.getDatasetsLengthChartJS chartId
    nodeName <- liftIO $ getDisplayedValue displayedElements nodeId (anId <> "__node-name")
    newColor <- getNewColor colors
    UI.runFunction $ UI.ffi Chart.addDatasetChartJS chartId (maybe anId id nodeName) newColor
    saveDatasetIx datasetIndices nodeId newIx
 where
  chartsIds :: [String]
  chartsIds =
    [ "cpu-chart"
    ]

-- | When we add points to chart, we have to remember the timestamp of the latest point,
--   for each chart, to avoid duplicated rendering of the same points.
type LatestTimestamps   = Map DataName POSIXTime
type DatasetsTimestamps = TVar (Map NodeId LatestTimestamps)

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

-- | ...
addPointsToChart
  :: String
  -> NodeId
  -> DatasetsIndices
  -> [(POSIXTime, ValueH)]
  -> UI ()
addPointsToChart _ _ _ [] = return ()
addPointsToChart chartId nodeId datasetIndices points =
  whenJustM (getDatasetIx datasetIndices nodeId) $ \datasetIx ->
    forM_ points $ \(ts, valueH) ->
      UI.runFunction $ UI.ffi Chart.addNewPointChartJS chartId (show ts) datasetIx (show valueH)
