{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.UI
  ( updateUI
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Control.Monad (forM_)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Charts
import           Cardano.Tracer.Handlers.RTView.Update.Nodes
import           Cardano.Tracer.Handlers.RTView.Update.Peers
import           Cardano.Tracer.Types

updateUI
  :: UI.Window
  -> ConnectedNodes
  -> DisplayedElements
  -> SavedTraceObjects
  -> DataPointRequestors
  -> PageReloadedFlag
  -> NonEmpty LoggingParams
  -> Colors
  -> DatasetsIndices
  -> UI ()
updateUI window connectedNodes displayedElements savedTO
         dpRequestors reloadFlag loggingConfig colors datasetIndices = do
  updateNodesUI
    window
    connectedNodes
    displayedElements
    dpRequestors
    reloadFlag
    loggingConfig
    colors
    datasetIndices
  --
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ (M.toList savedTraceObjects) $ \(nodeId, savedTOForNode) ->
    forM_ (M.toList savedTOForNode) $ \(namespace, trObValue) ->
      case namespace of
        "Cardano.Node.Peers" -> updatePeers window nodeId displayedElements trObValue
        "density" -> return () -- updateElement
        "slotNum" -> return ()
        "blockNum" -> return ()
        "slotInEpoch" -> return ()
        "epoch" -> return ()
        "forks" -> return ()
        "txsInMempool"  -> return ()
        "mempoolBytes"  -> return ()
        "txsProcessedNum"  -> return ()
        "blocksForgedNum"  -> return ()
        "nodeCannotForge"  -> return ()
        "nodeIsLeaderNum"  -> return ()
        "slotsMissedNum" -> return ()
        "operationalCertificateStartKESPeriod"  -> return ()
        "operationalCertificateExpiryKESPeriod"  -> return ()
        "currentKESPeriod"  -> return ()
        "remainingKESPeriods" -> return ()
        _ -> return ()

{-
updateElement = do
  let elId = ""
      elValue = trObValue
  liftIO (getDisplayedValue displayedElements nodeId elId) >>= \case
    Nothing ->
      -- There is no displayed value for this element yet.
      setAndSave elId elValue
    Just displayedValue ->
      -- There is a value that already displayed, check if it changed.
      unless (elValue == displayedValue) $
        setAndSave elId elValue
 where
   setAndSave elId elValue = do
    findAndSetText elValue window elId
    liftIO $ saveDisplayedValue displayedElements nodeId elId elValue
-}
