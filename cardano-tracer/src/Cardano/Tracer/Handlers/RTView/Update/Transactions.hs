{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Transactions
  ( updateTransactionsHistory
  ) where

import           Control.Monad.Extra (whenJust)
import           Data.Time.Clock
import           Data.Text (unpack)
import           Text.Read (readMaybe)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Types

updateTransactionsHistory
  :: NodeId
  -> TransactionsHistory
  -> MetricName
  -> MetricValue
  -> UTCTime
  -> IO ()
updateTransactionsHistory nodeId (TXHistory tHistory) metricName metricValue now =
  case metricName of
    "cardano.node.txsProcessedNum" -> updateTxsProcessedNum
    "cardano.node.mempoolBytes"    -> updateMempoolBytes
    "cardano.node.txsInMempool"    -> updateTxsInMempool
    _ -> return ()
 where
  valueS = unpack metricValue

  updateTxsProcessedNum =
    whenJust (readMaybe valueS) $ \(txsNum :: Integer) ->
      addHistoricalData tHistory nodeId now TxsProcessedNumData $ ValueI txsNum

  updateMempoolBytes =
    whenJust (readMaybe valueS) $ \(mempoolBytes :: Integer) ->
      addHistoricalData tHistory nodeId now MempoolBytesData $ ValueI mempoolBytes

  updateTxsInMempool =
    whenJust (readMaybe valueS) $ \(txsInMempool :: Integer) ->
      addHistoricalData tHistory nodeId now TxsInMempoolData $ ValueI txsInMempool
