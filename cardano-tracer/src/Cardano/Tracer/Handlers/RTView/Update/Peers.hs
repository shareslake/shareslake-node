{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.RTView.Update.Peers
  ( updatePeers
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core

import Debug.Trace

import           Cardano.Tracer.Handlers.RTView.State.Displayed
import           Cardano.Tracer.Handlers.RTView.UI.Utils
import           Cardano.Tracer.Types

updatePeers
  :: UI.Window
  -> NodeId
  -> DisplayedElements
  -> Text
  -> UI ()
updatePeers window (NodeId anId) _displayedElements trObValue =
  if "NodeKernelPeers" `T.isInfixOf` trObValue
    then return () -- It was empty 'TraceObject' (without useful info), ignore it.
    else
      case T.words trObValue of
        [addr, status, slotNo, reqsInF, blocksInF, bytesInF] -> do
          -- If we're here, there is some new information about peers:
          -- the new peer was connected, or some data for existing peer was changed.
          liftIO . traceIO $ "__CheckPeer11: " <> T.unpack trObValue
          -- 54.241.71.219   ready      ???      0      0       0
          -- 3.23.160.18     ready    4492799    1      1     636
          findAndSetText addr      window $ anId <> "__peer_addr"
          findAndSetText status    window $ anId <> "__peer_status"
          findAndSetText slotNo    window $ anId <> "__peer_slotNo"
          findAndSetText reqsInF   window $ anId <> "__peer_reqsInF"
          findAndSetText blocksInF window $ anId <> "__peer_blocksInF"
          findAndSetText bytesInF  window $ anId <> "__peer_bytesInF"
        _ -> do
          liftIO . traceIO $ "__CheckPeer22: " <> T.unpack trObValue
          return () -- It's strange: wrong format of peers info, ignore it.

{-
__CheckPeer11: 18.133.87.50    ready      ???    0      0       0
__CheckPeer11: 54.151.113.34   ready      ???    0      0       0
__CheckPeer11: 54.151.113.34   ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 18.158.118.230  ready      ???    0      0       0
__CheckPeer22: 18.159.88.186   ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer11: 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 52.57.122.128   ready      ???    0      0       0, 3.127.133.53    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer22: 18.158.147.46   ready      ???    0      0       0, 35.158.3.162    ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer11: 3.13.80.218     ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer22: 3.127.133.53    ready      ???    0      0       0, 52.57.122.128   ready      ???    0      0       0
__CheckPeer11: 18.159.88.186   ready      ???    0      0       0
__CheckPeer11: 18.159.88.186   ready      ???    0      0       0
__CheckPeer11: 18.159.88.186   ready      ???    0      0       0
-}

