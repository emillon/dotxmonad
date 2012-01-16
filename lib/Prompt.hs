{-# OPTIONS -Wall -W -Werror #-}

module Prompt ( searchEngineMap
              , myPromptSearch
              , browsePrompt
              , myXPC
              ) where

import qualified Data.Map as M

import XMonad
import XMonad.Actions.Search
import XMonad.Prompt
import XMonad.Prompt.Input
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

browseTo :: String -> X ()
browseTo url = do
  browser <- liftIO getBrowser
  spawn $ browser ++ " " ++ url

browsePrompt :: XPConfig -> X ()
browsePrompt xpc = inputPrompt xpc "URL " ?+ browseTo
