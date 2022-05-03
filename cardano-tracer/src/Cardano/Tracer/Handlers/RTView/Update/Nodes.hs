{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Nodes
  ( addColumnsForConnected
  , addDatasetsForConnected
  , checkNoNodesState
  , updateNodesUI
  ) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad (forM_, unless, when)
import           Control.Monad.Extra (whenJust, whenJustM)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import           Data.Set (Set, (\\))
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time.Calendar
import           Data.Time.Clock (UTCTime (..), addUTCTime, diffUTCTime)
import           Data.Time.Clock.System
import           Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           Text.Read (readMaybe)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Metrics.Utils
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.HTML.Node.Column
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Handlers.RTView.Update.NodeInfo
import           Cardano.Tracer.Types

updateNodesUI
  :: UI.Window
  -> ConnectedNodes
  -> DisplayedElements
  -> AcceptedMetrics
  -> SavedTraceObjects
  -> DataPointRequestors
  -> NonEmpty LoggingParams
  -> Colors
  -> DatasetsIndices
  -> UI ()
updateNodesUI window connectedNodes displayedElements acceptedMetrics
              savedTO dpRequestors loggingConfig colors datasetIndices = do
  (connected, displayedEls) <- liftIO . atomically $ (,)
    <$> readTVar connectedNodes
    <*> readTVar displayedElements
  -- Check connected/disconnected nodes since previous UI's update.
  let displayed = S.fromList $ M.keys displayedEls
  when (connected /= displayed) $ do
    let disconnected   = displayed \\ connected -- In 'displayed' but not in 'connected'.
        newlyConnected = connected \\ displayed -- In 'connected' but not in 'displayed'.
    deleteColumnsForDisconnected window connected disconnected
    addColumnsForConnected window newlyConnected loggingConfig
    checkNoNodesState window connected
    askNSetNodeInfo window dpRequestors newlyConnected displayedElements
    addDatasetsForConnected window newlyConnected colors datasetIndices displayedElements
    liftIO $ updateDisplayedElements displayedElements connected
  setUptimeForNodes window connected displayedElements
  setBlockReplayProgress window connected displayedElements acceptedMetrics
  setChunkValidationProgress window connected savedTO
  setLeadershipStats window connected displayedElements acceptedMetrics

addColumnsForConnected
  :: UI.Window
  -> Set NodeId
  -> NonEmpty LoggingParams
  -> UI ()
addColumnsForConnected window newlyConnected loggingConfig = do
  unless (S.null newlyConnected) $
    findAndShow window "main-table-container"
  forM_ newlyConnected $ addNodeColumn window loggingConfig

addDatasetsForConnected
  :: UI.Window
  -> Set NodeId
  -> Colors
  -> DatasetsIndices
  -> DisplayedElements
  -> UI ()
addDatasetsForConnected window newlyConnected colors datasetIndices displayedElements = do
  unless (S.null newlyConnected) $
    findAndShow window "main-charts-container"
  forM_ newlyConnected $ \nodeId ->
    addNodeDatasetsToCharts window nodeId colors datasetIndices displayedElements

deleteColumnsForDisconnected
  :: UI.Window
  -> Set NodeId
  -> Set NodeId
  -> UI ()
deleteColumnsForDisconnected window connected disconnected = do
  forM_ disconnected $ deleteNodeColumn window
  when (S.null connected) $ do
    findAndHide window "main-table-container"
    findAndHide window "main-charts-container"
  -- Please note that we don't remove historical data from charts
  -- for disconnected node. Because the user may want to see the
  -- historical data even for the node that already disconnected.

checkNoNodesState :: UI.Window -> Set NodeId -> UI ()
checkNoNodesState window connected =
  if S.null connected
    then do
      findAndShow window "no-nodes"
      findAndShow window "no-nodes-info"
    else do
      findAndHide window "no-nodes"
      findAndHide window "no-nodes-info"

setUptimeForNodes
  :: UI.Window
  -> Set NodeId
  -> DisplayedElements
  -> UI ()
setUptimeForNodes window connected displayedElements = do
  now <- systemToUTCTime <$> liftIO getSystemTime
  forM_ connected $ \nodeId@(NodeId anId) -> do
    let nodeStartElId  = anId <> "__node-start-time"
        nodeUptimeElId = anId <> "__node-uptime"
    whenJustM (liftIO $ getDisplayedValue displayedElements nodeId nodeStartElId) $ \tsRaw ->
      whenJust (readMaybe (T.unpack tsRaw) :: Maybe UTCTime) $ \startTime -> do
        let uptimeDiff = now `diffUTCTime` startTime
            uptime = uptimeDiff `addUTCTime` nullTime
            uptimeFormatted = formatTime defaultTimeLocale "%X" uptime
            daysNum = utctDay uptime `diffDays` utctDay nullTime
            uptimeWithDays = if daysNum > 0
                               -- Show days only if 'uptime' > 23:59:59.
                               then show daysNum <> "d " <> uptimeFormatted
                               else uptimeFormatted
        findAndSetText (T.pack uptimeWithDays) window nodeUptimeElId
 where
  nullTime = UTCTime (ModifiedJulianDay 0) 0

setBlockReplayProgress
  :: UI.Window
  -> Set NodeId
  -> DisplayedElements
  -> AcceptedMetrics
  -> UI ()
setBlockReplayProgress window connected displayedElements acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ connected $ \nodeId ->
    whenJust (M.lookup nodeId allMetrics) $ \(ekgStore, _) -> do
      metrics <- liftIO $ getListOfMetrics ekgStore
      whenJust (lookup "Block replay progress (%)" metrics) $ \metricValue ->
        updateBlockReplayProgress nodeId $ T.unpack metricValue
 where
  updateBlockReplayProgress nodeId@(NodeId anId) valueS =
    whenJust (readMaybe valueS) $ \(progressPct :: Double) -> do
      let progressPctS = T.pack $ show progressPct
          nodeBlockReplayElId = anId <> "__node-block-replay"
      liftIO (getDisplayedValue displayedElements nodeId nodeBlockReplayElId) >>= \case
        Nothing ->
          setAndRemember progressPctS nodeId nodeBlockReplayElId
        Just displayedProgress ->
          unless (progressPctS == displayedProgress) $
            setAndRemember progressPctS nodeId nodeBlockReplayElId

  setAndRemember progressPctS nodeId@(NodeId anId) nodeBlockReplayElId = do
    findAndSetText progressPctS window nodeBlockReplayElId
    liftIO $ saveDisplayedValue displayedElements nodeId nodeBlockReplayElId progressPctS
    when ("100" `T.isInfixOf` progressPctS) $ do
      let nodeBlockReplayPctElId = anId <> "__node-block-replay-pct"
      findAndSet (set UI.class_ "rt-view-percent-done") window nodeBlockReplayElId
      findAndSet (set UI.class_ "rt-view-percent-done") window nodeBlockReplayPctElId

setChunkValidationProgress
  :: UI.Window
  -> Set NodeId
  -> SavedTraceObjects
  -> UI ()
setChunkValidationProgress window connected savedTO = do
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ connected $ \nodeId@(NodeId anId) ->
    whenJust (M.lookup nodeId savedTraceObjects) $ \savedTOForNode -> do
      let nodeChunkValidationElId = anId <> "__node-chunk-validation"
      forM_ (M.toList savedTOForNode) $ \(namespace, trObValue) ->
        case namespace of
          "Cardano.Node.ChainDB.ImmDbEvent.ChunkValidation.ValidatedChunk" ->
            -- In this case we don't need to check if the value differs from displayed one,
            -- because this 'TraceObject' is forwarded only with new values, and after 100%
            -- the node doesn't forward it anymore.
            --
            -- Example: "Validated chunk no. 2262 out of 2423. Progress: 93.36%"
            case T.words trObValue of
              [_, _, _, current, _, _, from, _, progressPct] ->
                findAndSetHTML (T.init progressPct <> "&nbsp;%: no. " <> current <> " from " <> T.init from)
                               window nodeChunkValidationElId
              _ -> return ()
          "Cardano.Node.ChainDB.ImmDbEvent.ValidatedLastLocation" -> do
            findAndSetHTML "100.0&nbsp;%" window nodeChunkValidationElId
            findAndSet (set UI.class_ "rt-view-percent-done") window nodeChunkValidationElId
          _ -> return ()

setLeadershipStats
  :: UI.Window
  -> Set NodeId
  -> DisplayedElements
  -> AcceptedMetrics
  -> UI ()
setLeadershipStats _window connected _displayedElements acceptedMetrics = do
  allMetrics <- liftIO $ readTVarIO acceptedMetrics
  forM_ connected $ \nodeId ->
    whenJust (M.lookup nodeId allMetrics) $ \(ekgStore, _) -> do
      metrics <- liftIO $ getListOfMetrics ekgStore
      forM_ metrics $ \(metricName, _metricValue) ->
        case metricName of
          "cardano.node.forgedSlotLast"        -> return ()
          "cardano.node.forgedInvalidSlotLast" -> return ()
          "cardano.node.couldNotForgeSlotLast" -> return ()
          "cardano.node.adoptedSlotLast"       -> return ()
          "cardano.node.notAdoptedSlotLast"    -> return ()
          "cardano.node.aboutToLeadSlotLast"   -> return ()
          "cardano.node.nodeIsLeader"          -> return ()
          "cardano.node.nodeNotLeader"         -> return ()
          "cardano.node.nodeCannotForge"       -> return ()
          _ -> return ()
