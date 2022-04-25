module Cardano.Tracer.Handlers.RTView.UI.JS.Utils
  ( copyTextToClipboard
  , downloadCSVFile
  , selectOption
  ) where

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

import           Cardano.Tracer.Handlers.RTView.UI.Types

copyTextToClipboard :: String
copyTextToClipboard = concat
  [ "const listener = function(ev) {"
  , "  ev.preventDefault();"
  , "  ev.clipboardData.setData('text/plain', %1);"
  , "};"
  , "document.addEventListener('copy', listener);"
  , "document.execCommand('copy');"
  , "document.removeEventListener('copy', listener);"
  ]

downloadCSVFile :: String
downloadCSVFile = concat
  [ "var element = document.createElement('a');"
  , "element.setAttribute('href', 'data:application/csv;charset=utf-8,' + %2);"
  , "element.setAttribute('download', %1);"
  , "element.style.display = 'none';"
  , "document.body.appendChild(element);"
  , "element.click();"
  , "document.body.removeChild(element);"
  ]

selectOption
  :: String
  -> Index
  -> UI ()
selectOption selectId (Index ix) =
  UI.runFunction $ UI.ffi "document.getElementById(%1).selectedIndex = %2;" selectId ix'
 where
  ix' :: Int
  ix' = fromIntegral ix
