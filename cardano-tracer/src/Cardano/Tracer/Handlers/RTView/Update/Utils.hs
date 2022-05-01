{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.Tracer.Handlers.RTView.Update.Utils
  ( askDataPoint
  , utc2ns
  , utc2s
  ) where

import           Control.Concurrent.STM.TVar (readTVarIO)
import           Data.Aeson (FromJSON, decode')
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as M
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX
import           Data.Word (Word64)

import           Trace.Forward.Utils.DataPoint (askForDataPoints)
import           Trace.Forward.Protocol.DataPoint.Type (DataPointName)

import           Cardano.Tracer.Types

-- | There is a different information the node can provide us by explicit request.
--   This is a structured data about internal state of the node (for example, its
--   basic information like version, protocol, commit hash, start time, etc).
--
--   Such a structured data is provided as a 'DataPoint'. When it's receved, it's
--   technically a lazy bytestring that is a result of 'ToJSON'-encoding on the
--   forwarder's side. Here we can decode it to particular Haskell type provided
--   by the node.
askDataPoint
  :: FromJSON a
  => DataPointRequestors
  -> NodeId
  -> DataPointName
  -> IO (Maybe a)
askDataPoint dpRequestors nodeId dpName =
  readTVarIO dpRequestors <&> M.lookup nodeId >>= \case
    Nothing -> return Nothing
    Just dpRequestor ->
      askForDataPoints dpRequestor [dpName] <&> lookup dpName >>= \case
        Just (Just rawValue) -> return $ decode' rawValue
        _ -> return Nothing

-- | Converts a timestamp to seconds since Unix epoch.
utc2s :: UTCTime -> Word64
utc2s utc = fromInteger . round $ utcTimeToPOSIXSeconds utc

-- | Converts a timestamp to nanoseconds since Unix epoch.
utc2ns :: UTCTime -> Word64
utc2ns utc = fromInteger . round $ 1000_000_000 * utcTimeToPOSIXSeconds utc
