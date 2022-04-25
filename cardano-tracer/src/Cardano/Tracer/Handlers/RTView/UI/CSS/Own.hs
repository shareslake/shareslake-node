{-# LANGUAGE QuasiQuotes #-}

module Cardano.Tracer.Handlers.RTView.UI.CSS.Own
  ( ownCSS
  -- Helpers
  , backgroundDark
  , backgroundLight
  , borderDark
  , borderLight
  , greyDark
  , greyLight
  , hrefDark
  , hrefLight
  , nameDark
  , nameLight
  , textDark
  , textLight
  ) where

import           Data.String.QQ

-- | To avoid run-time dependency from the static content, embed own CSS in the page's header.
ownCSS :: String
ownCSS = [s|
html {
  height: 100%;
}

body {
  font-family: sans-serif;
  font-size: 22px;
  color: #1b2238;
  background-color: #131325;
  min-height: 100%;
}

code {
  color: #1d359f;
  padding: 0.11em 0.2em 0.11em;
  border-radius: 3px;
}

.pageloader {
  background: #2c2b3b !important;
  opacity: 0.95 !important;
}

.rt-view-href {
  color: #607bf7;
}

.rt-view-href:hover {
  color: #889cf5 !important;
  border-bottom: 1px solid #889cf5;
}

span[data-tooltip] {
  border-bottom: none !important;
}

.rt-view-top-bar-dark {
  background-color: #131325;
  color: whitesmoke;
  padding-top: 8px;
  padding-bottom: 2px;
  border-bottom: 1px solid #888;
}

.rt-view-top-bar-light {
  background-color: #efefef;
  color: #131325;
  padding-top: 8px;
  padding-bottom: 2px;
  border-bottom: 1px solid #dbdbdb;
}

.rt-view-own-info-box {
  background-color: #2c2b3b !important;
  color: whitesmoke !important;
}

.rt-view-cardano-logo svg {
  width: 48px;
  color: whitesmoke;
  margin-left: 5px;
}

.rt-view-name {
  color: whitesmoke;
  margin-left: 17px;
  margin-right: 6px;
  margin-bottom: 6px;
}

.rt-view-info-icon svg {
  width: 25px;
  padding-top: 2px;
  color: whitesmoke;
  cursor: pointer;
}

.rt-view-notify-icon svg {
  width: 23px;
  padding-top: 2px;
  color: whitesmoke;
  cursor: pointer;
}

.rt-view-theme-icon svg {
  width: 23px;
  padding-top: 2px;
  margin-right: 13px;
  color: whitesmoke;
  cursor: pointer;
}

.rt-view-copy-icon svg {
  width: 20px;
  color: whitesmoke;
  cursor: pointer;
}

.rt-view-logs-icon svg {
  width: 23px;
  padding-top: 2px;
  color: whitesmoke;
}

.rt-view-what-icon {
  text-align: left !important;
}

.rt-view-what-icon svg {
  width: 16px;
  margin-left: 8px;
  color: #aaa;
  cursor: pointer;
}

.rt-view-overview-icon svg {
  width: 18px;
  margin-right: 12px;
  color: #1fc1c3;
}

.rt-view-node-link-icon svg {
  width: 19px;
  margin-left: 9px;
  color: #1fc1c3;
}

.rt-view-ada-node-icon {
  color: #1fc1c3;
}

.rt-view-node-name-column {
  margin-top: 2px;
}

.tabs a {
  color: #888;
}

.tabs ul {
  border-bottom-width: 0px;
}

.tabs a:hover {
  border-bottom: 1px solid white;
  color: #aaa;
}

.tabs li.is-active a {
  border-bottom: 4px solid white;
  color: white;
}

.rt-view-no-nodes-icon svg {
  width: 70px;
  margin-top: 60px;
  margin-bottom: 40px;
  color: #677deb;
}

.rt-view-no-nodes-message {
  font-size: 22px;
  color: whitesmoke;
}

.rt-view-vspace-with-hr {

}

.table thead th {
  color: whitesmoke;
  font-weight: normal;
}

.rt-view-node-column-cell {
  min-width: 350px;
  column: whitesmoke;
}

.rt-view-main-table td {
  padding-top: 25px;
  padding-bottom: 25px;
}

.rt-view-main-table th {
  color: whitesmoke;
  border-bottom: 1px solid #444;
  vertical-align: middle;
}

.rt-view-main-table {
  background-color: #131325;
  color: whitesmoke;
}

.rt-view-main-table-description {
  min-width: 350px;
}

.rt-view-table-description-td {
  border-bottom: 1px solid #444;
}

.rt-view-peers-table-container {
  margin-left: 15px;
  margin-right: 15px;
}

.rt-view-peers-table {
  width: 100%;
  background-color: #2c2b3b;
  color: whitesmoke;
  font-size: 19px;
}

.rt-view-no-nodes-info {
  max-width: 600px !important;
  margin-top: 80px;
}

.rt-view-about-head {
  color: whitesmoke;
  background-color: #282841;
  border-bottom: 1px solid #555;
}

.rt-view-about-title {
  color: whitesmoke;
}

.rt-view-about-body {
  color: whitesmoke;
  background-color: #131325;
  border-bottom-left-radius: 6px;
  border-bottom-right-radius: 6px;
}

.rt-view-charts-container {
  
}

.rt-view-chart-container {
  background-color: #eeeeee;
  padding-top: 10px;
  padding-bottom: 20px;
  padding-left: 20px;
  padding-right: 20px;
  border: 1px solid #dddddd;
  border-radius: 6px;
}

.rt-view-chart-area {
  width: 100% !important;
}
|]

hrefLight
  , hrefDark
  , textLight
  , textDark
  , nameLight
  , nameDark
  , backgroundLight
  , backgroundDark
  , greyDark
  , greyLight
  , borderDark
  , borderLight :: String
hrefLight       = "#607bf7"
hrefDark        = "#264af0"
textLight       = "whitesmoke"
textDark        = "#333"
nameLight       = "whitesmoke"
nameDark        = "#0033ad"
backgroundLight = "whitesmoke"
backgroundDark  = "#131325"
greyDark        = "#282841"
greyLight       = "#efefef"
borderDark      = "1px solid #555 !important"
borderLight     = "1px solid #cfcfcf !important"
