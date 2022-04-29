{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Resources
  ( updateResourcesHistory
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import           Control.Monad.Extra (whenJust)
import qualified Data.Map.Strict as M
import           Data.Time.Clock.System
import           Graphics.UI.Threepenny.Core
import           Data.Text (unpack)
import           Data.Word (Word64)
import           Text.Read (readMaybe)

import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Historical
import           Cardano.Tracer.Handlers.RTView.State.Last
import           Cardano.Tracer.Handlers.RTView.Update.Utils
import           Cardano.Tracer.Types

updateResourcesHistory
  :: AcceptedMetrics
  -> ResourcesHistory
  -> LastResources
  -> IO ()
updateResourcesHistory acceptedMetrics (ResHistory rHistory) lastResources = do
  now <- systemToUTCTime <$> getSystemTime
  allMetrics <- readTVarIO acceptedMetrics
  forM_ (M.toList allMetrics) $ \(nodeId, (ekgStore, _)) -> do
    metrics <- liftIO $ getListOfMetrics ekgStore
    forM_ metrics $ \(metricName, metricValue) -> do
      let valueS = unpack metricValue
      case metricName of
        "stat.cputicks"    -> updateCPUUsage     nodeId valueS now
        "mem.resident"     -> updateRSSMemory    nodeId valueS now
        "rts.gcLiveBytes"  -> updateGCLiveMemory nodeId valueS now
        "rts.gcMajorNum"   -> updateGCMajorNum   nodeId valueS now
        "rts.gcMinorNum"   -> updateGCMinorNum   nodeId valueS now
        "rts.gcticks"      -> updateCPUTimeGC    nodeId valueS now
        "rts.mutticks"     -> updateCPUTimeApp   nodeId valueS now
        "rts.stat.threads" -> updateThreadsNum   nodeId valueS now
        _ -> return ()
 where
  updateCPUUsage nodeId valueS now =
    whenJust (readMaybe valueS) $ \(cpuTicks :: Integer) -> do
      lastOnes <- readTVarIO lastResources
      case M.lookup nodeId lastOnes of
        Nothing ->
          -- There is no last resources for this node yet.
          addNullResources lastResources nodeId
        Just resourcesForNode -> do
          let tns        = utc2ns now
              tDiffInSec = max 0.1 $ fromIntegral (tns - cpuLastNS resourcesForNode) / 1000_000_000 :: Double
              ticksDiff  = cpuTicks - cpuLastTicks resourcesForNode
              cpuV       = fromIntegral ticksDiff / fromIntegral (100 :: Integer) / tDiffInSec
              newCPUPct  = if cpuV < 0 then 0.0 else cpuV * 100.0
          addHistoricalData rHistory nodeId now CPUData $ ValueD newCPUPct
          updateLastResources lastResources nodeId $ \current ->
            current { cpuLastTicks = cpuTicks
                    , cpuLastNS = tns
                    }

  updateRSSMemory nodeId valueS now =
    whenJust (readMaybe valueS) $ \(bytes :: Word64) -> do
      let memoryInMB = fromIntegral bytes / 1024 / 1024 :: Double
      addHistoricalData rHistory nodeId now MemoryData $ ValueD memoryInMB

  updateGCLiveMemory nodeId valueS now =
    whenJust (readMaybe valueS) $ \(bytes :: Word64) -> do
      let memoryInMB = fromIntegral bytes / 1024 / 1024 :: Double
      addHistoricalData rHistory nodeId now GCLiveMemoryData $ ValueD memoryInMB

  updateGCMajorNum nodeId valueS now =
    whenJust (readMaybe valueS) $ \(gcMajorNum :: Integer) ->
      addHistoricalData rHistory nodeId now GCMajorNumData $ ValueI gcMajorNum

  updateGCMinorNum nodeId valueS now =
    whenJust (readMaybe valueS) $ \(gcMinorNum :: Integer) ->
      addHistoricalData rHistory nodeId now GCMinorNumData $ ValueI gcMinorNum

  updateCPUTimeGC nodeId valueS now =
    whenJust (readMaybe valueS) $ \(cpuTimeGCInCentiS :: Word64) -> do
      -- This is a total CPU time used by the GC, as 1/100 second.
      let cpuTimeGCInMs = cpuTimeGCInCentiS * 10
      addHistoricalData rHistory nodeId now CPUTimeGCData $ ValueI (fromIntegral cpuTimeGCInMs)

  updateCPUTimeApp nodeId valueS now =
    whenJust (readMaybe valueS) $ \(cpuTimeAppInCentiS :: Word64) -> do
      -- This is a total CPU time used by the the node itself, as 1/100 second.
      let cpuTimeAppInMs = cpuTimeAppInCentiS * 10
      addHistoricalData rHistory nodeId now CPUTimeAppData $ ValueI (fromIntegral cpuTimeAppInMs)

  updateThreadsNum nodeId valueS now =
    whenJust (readMaybe valueS) $ \(threadsNum :: Integer) ->
      addHistoricalData rHistory nodeId now ThreadsNumData $ ValueI threadsNum
