{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.Theme
  ( restoreTheme 
  , switchTheme
  ) where

import           Control.Exception.Extra (ignore, try_)
import           Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import           System.Directory

import           Cardano.Tracer.Handlers.RTView.UI.CSS.Own
import           Cardano.Tracer.Handlers.RTView.UI.Img.Icons
import           Cardano.Tracer.Handlers.RTView.UI.Utils

restoreTheme, switchTheme :: UI.Window -> UI ()
restoreTheme window = readSavedTheme >>= setThemeAndSave window
switchTheme window  = readSavedTheme >>= setThemeAndSave window . switch
 where
  switch s = if s == darkState then lightState else darkState

setThemeAndSave
  :: UI.Window
  -> String
  -> UI ()
setThemeAndSave window themeToSet = do
  let toBeLight = themeToSet == lightState
  changeThemeIcon toBeLight
  changeBodyBackground toBeLight
  changeTopNavigation toBeLight
  saveTheme themeToSet
 where
  changeThemeIcon toBeLight = do
    findAndSet ( (set html (if toBeLight then rtViewThemeToDarkSVG else rtViewThemeToLightSVG))
               . (set dataState (if toBeLight then lightState else darkState))
               . (set dataTooltip ("Switch to " <> (if toBeLight then "dark" else "light") <> " theme"))
               ) window "theme-icon"

  changeBodyBackground toBeLight =
    getElementsByTagName window "body" >>= \case
      [pageBody] -> void $
        element pageBody
          # set style [("background-color", if toBeLight then backgroundLight else backgroundDark)]
      _ -> return ()

  changeTopNavigation toBeLight = do
    findAndSet (set UI.class_ (if toBeLight
                                 then "navbar rt-view-top-bar-light"
                                 else "navbar rt-view-top-bar-dark")) window "top-bar"
    findAndSet (set UI.html (if toBeLight
                               then cardanoLogoDarkSVG
                               else cardanoLogoLightSVG)) window "cardano-logo"
    findAndSet (set UI.html (if toBeLight
                               then rtViewInfoDarkSVG
                               else rtViewInfoLightSVG)) window "info-icon"
    findAndSet (set UI.html (if toBeLight
                               then rtViewNotifyDarkSVG
                               else rtViewNotifyLightSVG)) window "notify-icon"
    findAndSet (set UI.html (if toBeLight
                               then noNodesDarkSVG
                               else noNodesLightSVG)) window "no-nodes-icon"
    findAndSet (set style [("color", if toBeLight
                                       then nameDark
                                       else nameLight)]) window "name"
    findAndSet (set style [("color", if toBeLight
                                       then nameDark
                                       else nameLight)]) window "no-nodes-message"
    findAndSet (set style [ ("background-color", if toBeLight
                                                   then backgroundLight
                                                   else backgroundDark)
                          , ("color", if toBeLight
                                        then textDark
                                        else textLight)
                          ]) window "main-table" 
    findByClassAndSet (set style [("color", if toBeLight
                                              then textDark
                                              else textLight)]) window "rt-view-node-column-cell"
    findByClassAndSet (set style [("color", if toBeLight
                                              then hrefDark
                                              else hrefLight)]) window "rt-view-href"
    findByClassAndSet (set UI.class_ (if toBeLight
                                        then "tag is-info is-rounded mr-3 has-tooltip-multiline has-tooltip-top rt-view-logs-path"
                                        else "tag is-info is-light is-rounded mr-3 has-tooltip-multiline has-tooltip-top rt-view-logs-path"))
                      window "rt-view-logs-path"
    findByClassAndSet (set UI.class_ (if toBeLight
                                        then "tag is-warning is-rounded ml-3 has-tooltip-multiline has-tooltip-top rt-view-logs-format"
                                        else "tag is-warning is-light is-rounded ml-3 has-tooltip-multiline has-tooltip-top rt-view-logs-format"))
                      window "rt-view-logs-format"
    findByClassAndSet (set UI.html (if toBeLight
                                      then copyDarkSVG
                                      else copyLightSVG)) window "rt-view-copy-icon"
    -- About modal
    findByClassAndSet (set style [ ("background-color", if toBeLight
                                                          then backgroundLight
                                                          else backgroundDark)
                                 , ("color", if toBeLight
                                              then textDark
                                              else textLight)
                                 ]
                      ) window "rt-view-about-body"
    findByClassAndSet (set style [ ("background-color", if toBeLight
                                                          then greyLight
                                                          else greyDark)
                                 , ("color", if toBeLight
                                              then textDark
                                              else textLight)
                                 , ("border-bottom", if toBeLight
                                                       then borderLight
                                                       else borderDark)
                                 ]
                      ) window "rt-view-about-head"
    findByClassAndSet (set style [("color", if toBeLight
                                              then textDark
                                              else textLight)]) window "rt-view-about-title"

lightState, darkState :: String
lightState = "light"
darkState  = "dark"

-- | Every time when the user changed the theme, it should be saved on the file
--   for next sessions, both after web-page reload and 'cardano-tracer' restart.
saveTheme :: String -> UI ()
saveTheme state = liftIO . ignore $ do
  pathToThemeConfig <- getPathToThemeConfig
  TIO.writeFile pathToThemeConfig $ T.pack state
  
readSavedTheme :: UI String
readSavedTheme = liftIO $
  try_ (TIO.readFile =<< getPathToThemeConfig) >>= \case
    Left _      -> return darkState -- Something is wrong, use default dark theme.
    Right saved -> return $ T.unpack saved

getPathToThemeConfig :: IO FilePath
getPathToThemeConfig = getXdgDirectory XdgConfig "rt-view-theme-config"
