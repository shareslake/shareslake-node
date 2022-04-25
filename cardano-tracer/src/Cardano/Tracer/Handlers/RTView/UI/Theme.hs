{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Tracer.Handlers.RTView.UI.Theme
  ( restoreTheme
  , switchTheme
  , isCurrentThemeDark
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

isCurrentThemeDark :: UI Bool
isCurrentThemeDark = do
  currentTheme <- readSavedTheme
  return $ currentTheme == darkState

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
    change "theme-icon" $
        (set html        (if toBeLight then rtViewThemeToDarkSVG else rtViewThemeToLightSVG))
      . (set dataState   (if toBeLight then lightState else darkState))
      . (set dataTooltip ("Switch to " <> (if toBeLight then "dark" else "light") <> " theme"))

  changeBodyBackground toBeLight =
    getElementsByTagName window "body" >>= \case
      [pageBody] -> void $
        element pageBody
          # set style [("background-color", if toBeLight
                                              then backgroundLight
                                              else backgroundDark)]
      _ -> return ()

  changeTopNavigation toBeLight = do
    changeClass "top-bar" $
      if toBeLight
        then "navbar rt-view-top-bar-light"
        else "navbar rt-view-top-bar-dark"
    changeIcon "cardano-logo" $
      if toBeLight
        then cardanoLogoDarkSVG
        else cardanoLogoLightSVG
    changeIcon "info-icon" $
      if toBeLight
        then rtViewInfoDarkSVG
        else rtViewInfoLightSVG
    changeIcon "notify-icon" $
      if toBeLight
        then rtViewNotifyDarkSVG
        else rtViewNotifyLightSVG
    changeIcon "no-nodes-icon" $
      if toBeLight
        then noNodesDarkSVG
        else noNodesLightSVG
    changeIcon' "rt-view-copy-icon" $
      if toBeLight
        then copyDarkSVG
        else copyLightSVG
    changeStyle "name"
      [ ("color", if toBeLight
                    then nameDark
                    else nameLight)
      ]
    changeStyle "no-nodes-message"
      [ ("color", if toBeLight
                    then nameDark
                    else nameLight)
      ]
    changeStyle "main-table"
      [ ("background-color", if toBeLight
                               then backgroundLight
                               else backgroundDark)
      , ("color", if toBeLight
                    then textDark
                    else textLight)
      ]
    changeStyle' "rt-view-node-name"
      [ ("color", if toBeLight
                    then textDark
                    else textLight)
      ]
    changeStyle' "rt-view-node-column-cell"
      [ ("color", if toBeLight
                    then textDark
                    else textLight)
      ]
    changeStyle' "rt-view-href"
      [ ("color", if toBeLight
                    then hrefDark
                    else hrefLight)
      ]
    changeClass' "rt-view-logs-path" $
      if toBeLight
        then "tag is-info is-rounded mr-3 has-tooltip-multiline has-tooltip-top rt-view-logs-path"
        else "tag is-info is-light is-rounded mr-3 has-tooltip-multiline has-tooltip-top rt-view-logs-path"
    changeClass' "rt-view-logs-format" $
      if toBeLight
        then "tag is-warning is-rounded ml-3 has-tooltip-multiline has-tooltip-top rt-view-logs-format"
        else "tag is-warning is-light is-rounded ml-3 has-tooltip-multiline has-tooltip-top rt-view-logs-format"
    changeStyle' "rt-view-table-description-td"
      [ ("border-bottom", if toBeLight
                            then borderLight
                            else borderDark)
      ]
    changeStyle' "rt-view-node-column-cell"
      [ ("border-bottom", if toBeLight
                            then borderLight
                            else borderDark)
      ]
    -- 'About' modal.
    changeStyle' "rt-view-about-body"
      [ ("background-color", if toBeLight
                               then backgroundLight
                               else backgroundDark)
      , ("color", if toBeLight
                    then textDark
                    else textLight)
      ]
    changeStyle' "rt-view-about-head"
      [ ("background-color", if toBeLight
                               then greyLight
                               else greyDark)
      , ("color", if toBeLight
                    then textDark
                    else textLight)
      , ("border-bottom", if toBeLight
                            then borderLight
                            else borderDark)
      ]
    changeStyle' "rt-view-about-title"
      [ ("color", if toBeLight
                    then textDark
                    else textLight)
      ]

  changeIcon  icon what = change  icon $ set UI.html what
  changeIcon' icon what = change' icon $ set UI.html what

  changeStyle  elId    s = change  elId    $ set style s
  changeStyle' classId s = change' classId $ set style s

  changeClass  elId    c = change  elId    $ set UI.class_ c
  changeClass' classId c = change' classId $ set UI.class_ c

  change  elId    what = findAndSet        what window elId
  change' classId what = findByClassAndSet what window classId

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
    Right saved -> return $ T.unpack saved
    Left _      -> return darkState -- Use dark theme by default.

getPathToThemeConfig :: IO FilePath
getPathToThemeConfig = getXdgDirectory XdgConfig "rt-view-theme-config"
