{-# OPTIONS -Wall -W -Werror #-}

module Prompt ( searchUsingMap
              , selectSearchUsingMap
              ) where

import qualified Data.Map as M

import Data.Char

import XMonad
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Prompt
import XMonad.Prompt.Shell

myXPC :: XPConfig
myXPC = amberXPConfig

searchEngineMap :: (SearchEngine -> a) -> M.Map (KeyMask, KeySym) a
searchEngineMap method =
  M.fromList $ map (\ (k, m) -> ((0, k), method m)) $
    [ (xK_d, duckduckgo)
    , (xK_g, googleSSL)
    , (xK_h, hoogle)
    , (xK_w, wikipedia)
    , (xK_y, youtube)
    , (xK_r, wordReference)
    , (xK_b, debbts)
    , (xK_p, debpts)
    , (xK_o, ocaml)
    , (xK_c, cpan)
    ]

googleSSL :: SearchEngine
googleSSL = searchEngine "google" "https://www.google.com/search?num=100&q="

duckduckgo :: SearchEngine
duckduckgo = searchEngine "ddg" "http://duckduckgo.com/?q="

wordReference :: SearchEngine
wordReference = searchEngine "wordreference" "http://wordreference.com/enfr/"

ocaml :: SearchEngine
ocaml =
  searchEngineF "ocaml" (\s -> pfx ++ escape (ucFirst s) ++ ".html")
    where
      pfx = "file:///usr/share/doc/ocaml-doc/ocaml.html/libref/"

ucFirst :: String -> String
ucFirst [] = []
ucFirst (x:xs) = toUpper x : xs

cpan :: SearchEngine
cpan = searchEngine "cpan" "http://search.cpan.org/search?query="

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
