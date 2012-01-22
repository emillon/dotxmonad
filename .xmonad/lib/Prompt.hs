{-# OPTIONS -Wall -W -Werror #-}

module Prompt ( searchUsingMap
              , selectSearchUsingMap
              ) where

import qualified Data.Map as M

import XMonad
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Prompt
import XMonad.Prompt.Shell

myXPC :: XPConfig
myXPC = amberXPConfig

searchEngineMap :: (SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
searchEngineMap method = M.fromList
                             [ ((0, xK_d), method duckduckgo)
                             , ((0, xK_g), method googleSSL)
                             , ((0, xK_h), method hoogle)
                             , ((0, xK_w), method wikipedia)
                             , ((0, xK_y), method youtube)
                             , ((0, xK_r), method wordReference)
                             ]

googleSSL :: SearchEngine
googleSSL = searchEngine "google" "https://www.google.com/search?num=100&q="

duckduckgo :: SearchEngine
duckduckgo = searchEngine "ddg" "http://duckduckgo.com/?q="

wordReference :: SearchEngine
wordReference = searchEngine "wordreference" "http://wordreference.com/enfr/"

data SearchPrompt = SearchPrompt String
instance XPrompt SearchPrompt where
    showXPrompt (SearchPrompt name)= "Search [" ++ name ++ "]: "

myPromptSearch :: XPConfig -> SearchEngine -> X ()
myPromptSearch xpc (SearchEngine name site) = do
  browser <- liftIO getBrowser
  mkXPrompt (SearchPrompt name) xpc (const $ return []) $ search browser site

searchUsingMap :: X ()
searchUsingMap =
  submap . searchEngineMap $ myPromptSearch myXPC

selectSearchUsingMap :: X ()
selectSearchUsingMap =
  submap . searchEngineMap $ selectSearch
