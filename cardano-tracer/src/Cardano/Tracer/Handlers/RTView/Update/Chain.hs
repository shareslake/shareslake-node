{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Chain
  ( updateBlockchainHistory
  ) where

import           Control.Monad.Extra (whenJust)
import           Data.Time.Clock
import           Data.Text (unpack)
import           Text.Read (readMaybe)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Types

updateBlockchainHistory
  :: NodeId
  -> BlockchainHistory
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateBlockchainHistory nodeId (ChainHistory cHistory) metricName metricValue now =
  case metricName of
    "cardano.node.density"     -> updateChainDensity
    "cardano.node.slotNum"     -> updateSlotNum
    "cardano.node.blockNum"    -> updateBlockNum
    "cardano.node.slotInEpoch" -> updateSlotInEpoch
    "cardano.node.epoch"       -> updateEpoch
    _ -> return ()
 where
  valueS = unpack metricValue

  updateChainDensity =
    whenJust (readMaybe valueS) $ \(density :: Double) -> do
      let !density' = 0.05 + density * 100.0
      addHistoricalData cHistory nodeId now ChainDensityData $ ValueD density'

  updateSlotNum =
    whenJust (readMaybe valueS) $ \(slotNum :: Integer) ->
      addHistoricalData cHistory nodeId now SlotNumData $ ValueI slotNum

  updateBlockNum =
    whenJust (readMaybe valueS) $ \(blockNum :: Integer) ->
      addHistoricalData cHistory nodeId now BlockNumData $ ValueI blockNum

  updateSlotInEpoch =
    whenJust (readMaybe valueS) $ \(slotInEpoch :: Integer) ->
      addHistoricalData cHistory nodeId now SlotInEpochData $ ValueI slotInEpoch

  updateEpoch =
    whenJust (readMaybe valueS) $ \(epoch :: Integer) ->
      addHistoricalData cHistory nodeId now EpochData $ ValueI epoch
