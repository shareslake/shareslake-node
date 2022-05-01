{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.UI.JS.Utils
  ( copyTextToClipboard
  , downloadCSVFile
  , selectOption
  ) where

import           Data.String.QQ
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI

copyTextToClipboard :: String -> UI ()
copyTextToClipboard textToCopy =
  UI.runFunction $ UI.ffi copyTextToClipboard' textToCopy

copyTextToClipboard' :: String
copyTextToClipboard' = [s|
const listener = function(ev) {
  ev.preventDefault();
  ev.clipboardData.setData('text/plain', %1);
};
document.addEventListener('copy', listener);
document.execCommand('copy');
document.removeEventListener('copy', listener);
|]

downloadCSVFile :: String
downloadCSVFile = [s|
var element = document.createElement('a');
element.setAttribute('href', 'data:application/csv;charset=utf-8,' + %2);
element.setAttribute('download', %1);
element.style.display = 'none';
document.body.appendChild(element);
element.click();
document.body.removeChild(element);
|]

selectOption
  :: String
  -> Int
  -> UI ()
selectOption selectId optionValue =
  UI.runFunction $ UI.ffi "document.getElementById(%1).value = %2;" selectId optionValue
