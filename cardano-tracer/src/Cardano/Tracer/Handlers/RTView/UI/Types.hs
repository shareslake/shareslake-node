{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Cardano.Tracer.Handlers.RTView.UI.Types
  ( ChartId (..)
  , ChartSelectId (..)
  , ChartSettings (..)
  , ChartsSettings
  , Color (..)
  , Colors
  , DatasetsIndices
  , DatasetsTimestamps
  , Index (..)
  ) where

import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Concurrent.STM.TVar (TVar)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Map.Strict (Map)
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           Cardano.Tracer.Types (NodeId (..))
import           Cardano.Tracer.Handlers.RTView.State.Historical

data ChartId
  = CPUChart
  | MemoryChart
  | GCMajorNumChart
  | GCMinorNumChart
  | GCLiveMemoryChart
  | CPUTimeGCChart
  | CPUTimeAppChart
  | ThreadsNumChart
  -- Chain
  | ChainDensityChart
  | SlotNumChart
  | BlockNumChart
  | SlotInEpochChart
  | EpochChart
  deriving (Bounded, Enum, Generic, FromJSON, ToJSON, Show)

data ChartSelectId
  = TimeFormatSelect
  | TimeUnitSelect
  deriving Show

newtype Index = Index Word8
  deriving (Generic, FromJSON, ToJSON)

data ChartSettings = ChartSettings
  { csTimeFormatIx :: !Index
  , csTimeUnitIx   :: !Index
  } deriving (Generic, FromJSON, ToJSON)

type ChartsSettings = [(ChartId, ChartSettings)]

newtype Color = Color String

type Colors = TBQueue Color

-- | After the node is connected, we have to add a new dataset to all historical charts.
--   The metrics received from this node will be added in these datasets.
--   Since each dataset has its index, we need a map 'NodeId -> ix',
--   where 'ix' is an index of a dataset in _each_ chart.
type DatasetsIndices = TVar (Map NodeId Index)

-- | When we add points to chart, we have to remember the timestamp of the latest point,
--   for each chart, to avoid duplicated rendering of the same points.
type LatestTimestamps   = Map DataName POSIXTime
type DatasetsTimestamps = TVar (Map NodeId LatestTimestamps)
