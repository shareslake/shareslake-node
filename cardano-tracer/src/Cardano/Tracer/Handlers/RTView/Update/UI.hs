{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.UI
  ( updateUI
  , updateUIAfterReload
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad (forM_)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as M
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.State.TraceObjects
import           Cardano.Tracer.Handlers.RTView.UI.Types
import           Cardano.Tracer.Handlers.RTView.Update.NodeInfo
import           Cardano.Tracer.Handlers.RTView.Update.Nodes
import           Cardano.Tracer.Handlers.RTView.Update.Peers
import           Cardano.Tracer.Types

updateUI
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
updateUI window connectedNodes displayedElements acceptedMetrics savedTO
         dpRequestors loggingConfig colors datasetIndices = do
  updateNodesUI
    window
    connectedNodes
    displayedElements
    acceptedMetrics
    dpRequestors
    loggingConfig
    colors
    datasetIndices
  --
  savedTraceObjects <- liftIO $ readTVarIO savedTO
  forM_ (M.toList savedTraceObjects) $ \(nodeId, savedTOForNode) ->
    forM_ (M.toList savedTOForNode) $ \(namespace, trObValue) ->
      case namespace of
        "Cardano.Node.Peers" -> updatePeers window nodeId displayedElements trObValue
        _ -> return ()

updateUIAfterReload
  :: UI.Window
  -> ConnectedNodes
  -> DisplayedElements
  -> DataPointRequestors
  -> NonEmpty LoggingParams
  -> Colors
  -> DatasetsIndices
  -> UI ()
updateUIAfterReload window connectedNodes displayedElements
                    dpRequestors loggingConfig colors datasetIndices = do
  -- Ok, web-page was reload (i.e. it's the first update after DOM-rendering),
  -- so displayed state should be restored immediately.
  connected <- liftIO $ readTVarIO connectedNodes  
  addColumnsForConnected window connected loggingConfig
  checkNoNodesState window connected
  askNSetNodeInfo window dpRequestors connected displayedElements
  addDatasetsForConnected window connected colors datasetIndices displayedElements
  liftIO $ updateDisplayedElements displayedElements connected
