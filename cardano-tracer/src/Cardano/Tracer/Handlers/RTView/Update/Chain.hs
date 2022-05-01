{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Chain
  ( updateBlockchainHistory
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import           Data.Time.Clock.System
import           Graphics.UI.Threepenny.Core
import           Data.Text (unpack)
import           Text.Read (readMaybe)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Types

updateBlockchainHistory
  :: AcceptedMetrics
  -> BlockchainHistory
  -> IO ()
updateBlockchainHistory acceptedMetrics (ChainHistory cHistory) = do
  now <- systemToUTCTime <$> getSystemTime
  allMetrics <- readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(nodeId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(metricName, metricValue) -> do
      let valueS = unpack metricValue
      case metricName of
        "cardano.node.density"     -> updateChainDensity nodeId valueS now
        "cardano.node.slotNum"     -> updateSlotNum      nodeId valueS now
        "cardano.node.blockNum"    -> updateBlockNum     nodeId valueS now
        "cardano.node.slotInEpoch" -> updateSlotInEpoch  nodeId valueS now
        "cardano.node.epoch"       -> updateEpoch        nodeId valueS now
        _ -> return ()
 where
  updateChainDensity nodeId valueS now =
    whenJust (readMaybe valueS) $ \(density :: Double) -> do
      let !density' = 0.05 + density * 100.0
      addHistoricalData cHistory nodeId now ChainDensityData $ ValueD density'

  updateSlotNum nodeId valueS now =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now SlotNumData $ ValueI slotNum

  updateBlockNum nodeId valueS now =
    whenJust (readMaybe valueS) $ \(blockNum :: Integer) ->
      addHistoricalData cHistory nodeId now BlockNumData $ ValueI blockNum

  updateSlotInEpoch nodeId valueS now =
    whenJust (readMaybe valueS) $ \(slotInEpoch :: Integer) ->
      addHistoricalData cHistory nodeId now SlotInEpochData $ ValueI slotInEpoch

  updateEpoch nodeId valueS now =
    whenJust (readMaybe valueS) $ \(epoch :: Integer) ->
      addHistoricalData cHistory nodeId now EpochData $ ValueI epoch
