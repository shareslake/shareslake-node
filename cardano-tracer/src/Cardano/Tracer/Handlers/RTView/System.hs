{-# LANGUAGE CPP #-}

module Cardano.Tracer.Handlers.RTView.System
  ( getPathToChartsConfig
  , getPathToThemeConfig
  , getProcessId
  ) where

import           Data.Word (Word32)
import           Graphics.UI.Threepenny.Core
import           System.Directory
import           System.FilePath ((</>))

#if defined(mingw32_HOST_OS)
import           System.Win32.Process (getCurrentProcessId)
#else
import           System.Posix.Process (getProcessID)
import           System.Posix.Types (CPid (..))
#endif

getProcessId :: UI Word32
getProcessId =
#if defined(mingw32_HOST_OS)
  liftIO getCurrentProcessId 
#else
  do CPid pid <- liftIO getProcessID
     return $ fromIntegral pid
#endif

getPathToChartsConfig, getPathToThemeConfig :: IO FilePath
getPathToChartsConfig = getXdgDirectory XdgConfig $ rtViewConfigsRoot </> "charts"
getPathToThemeConfig  = getXdgDirectory XdgConfig $ rtViewConfigsRoot </> "theme"

rtViewConfigsRoot :: FilePath
rtViewConfigsRoot = "cardano-rt-view"
