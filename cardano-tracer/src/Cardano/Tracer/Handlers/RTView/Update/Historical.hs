{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Historical
  ( runHistoricalUpdater
  ) where

--import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forever) -- , forM_)
--import qualified Data.Map.Strict as M
import           System.Time.Extra (sleep)

--import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
--import           Cardano.Tracer.Handlers.RTView.Update.Nodes
--import           Cardano.Tracer.Handlers.RTView.Update.Peers
import           Cardano.Tracer.Handlers.RTView.Update.Chain
import           Cardano.Tracer.Handlers.RTView.Update.Resources
import           Cardano.Tracer.Types

-- | A lot of information received from the node is useful as historical data.
--   It means that such an information should be displayed on time charts,
--   where X axis is a time in UTC. An example: resource metrics, chain information,
--   tx information, etc.
--
--   This information is extracted both from 'TraceObject's and 'EKG.Metrics' and then
--   it will be saved as chart coords '[(ts, v)]', where 'ts' is a timestamp
--   and 'v' is a value. Later, when the user will open RTView web-page, this
--   saved data will be used to render historical charts.
--
--   It allows to collect historical data even when RTView web-page is closed.
runHistoricalUpdater
  :: SavedTraceObjects
  -> AcceptedMetrics
  -> ResourcesHistory
  -> LastResources
  -> BlockchainHistory
  -> IO ()
runHistoricalUpdater _savedTO acceptedMetrics resourcesHistory
                     lastResources chainHistory = forever $ do
  updateResourcesHistory acceptedMetrics resourcesHistory lastResources
  updateBlockchainHistory acceptedMetrics chainHistory
  sleep 1.0 -- TODO: should it be configured?
