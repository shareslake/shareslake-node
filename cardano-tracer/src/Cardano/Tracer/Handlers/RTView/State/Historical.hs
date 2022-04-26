{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.State.Historical
  ( BlockchainHistory (..)
  , DataName (..)
  , POSIXTime
  , ResourcesHistory (..)
  , TransactionsHistory (..)
  , ValueH (..)
  , addHistoricalData
  , getHistoricalData
  , initBlockchainHistory
  , initResourcesHistory
  , initTransactionsHistory
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Time.Clock (UTCTime)
import           Data.Word (Word64)

import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types (NodeId)

-- | A lot of information received from the node is useful as historical data.
--   It means that such an information should be displayed on time charts,
--   where X axis is a time in UTC. An example: resource metrics, chain information,
--   tx information, etc.
type POSIXTime = Word64

data ValueH
  = ValueD Double
  | ValueI Integer
  deriving (Eq, Ord)

instance Show ValueH where
  show (ValueD d) = show d
  show (ValueI i) = show i

type HistoricalPoints = Set (POSIXTime, ValueH)

-- | Historical points for particular data (for example, "CPU").
data DataName
  = CPUData
  | MemoryData
  | GCMajorNumData
  | GCMinorNumData
  deriving (Eq, Ord)

type HistoricalData = Map DataName HistoricalPoints
type History        = TVar (Map NodeId HistoricalData)

newtype BlockchainHistory   = ChainHistory History
newtype ResourcesHistory    = ResHistory   History
newtype TransactionsHistory = TXHistory    History

initBlockchainHistory :: IO BlockchainHistory
initBlockchainHistory = ChainHistory <$> newTVarIO M.empty

initResourcesHistory :: IO ResourcesHistory
initResourcesHistory = ResHistory <$> newTVarIO M.empty

initTransactionsHistory :: IO TransactionsHistory
initTransactionsHistory = TXHistory <$> newTVarIO M.empty

addHistoricalData
  :: History
  -> NodeId
  -> UTCTime
  -> DataName
  -> ValueH
  -> IO ()
addHistoricalData history nodeId now dataName valueH = atomically $
  modifyTVar' history $ \currentHistory ->
    case M.lookup nodeId currentHistory of
      Nothing ->
        -- There is no historical data for this node yet.
        let firstPoint = S.singleton (utc2s now, valueH)
            newDataForNode = M.singleton dataName firstPoint
        in M.insert nodeId newDataForNode currentHistory
      Just dataForNode ->
        let newDataForNode =
              case M.lookup dataName dataForNode of
                Nothing ->
                  -- There is no historical points for this dataName yet.
                  let firstPoint = S.singleton (utc2s now, valueH)
                  in M.insert dataName firstPoint dataForNode
                Just points ->
                  let pointsWeKeep = S.fromList . deleteOutdated . S.toAscList $ points
                      newPoints = S.insert (utc2s now, valueH) pointsWeKeep
                  in M.adjust (const newPoints) dataName dataForNode
        in M.adjust (const newDataForNode) nodeId currentHistory
 where
  -- All points that older than 'minAge' should be deleted.
  deleteOutdated [] = []
  deleteOutdated (point@(tsInSec, _):otherPoints) =
    if tsInSec < minAge
      then
        -- This point is too old, do not keep it anymore.
        deleteOutdated otherPoints
      else
        -- This point should be kept.
        -- Since the points were converted to asc list, all the next points
        -- are definitely newer (have bigger 'tsInSec'), so there is no need
        -- to check them.
        point : otherPoints

  minAge = utc2s now - pointsAgeInSec
  pointsAgeInSec = 12 * 60 * 60

getHistoricalData
  :: History
  -> NodeId
  -> DataName
  -> IO [(POSIXTime, ValueH)]
getHistoricalData history nodeId dataName = do
  history' <- readTVarIO history
  case M.lookup nodeId history' of
    Nothing -> return []
    Just dataForNode ->
      case M.lookup dataName dataForNode of
        Nothing -> return []
        Just points -> return $ S.toAscList points
