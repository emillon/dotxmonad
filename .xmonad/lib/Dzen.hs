{-# OPTIONS -Wall -W -Werror #-}

module Dzen(myLogHook) where

import System.IO

import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Util.Font
import XMonad.Util.Loggers

import Clock
import Utils

volume :: Logger
volume =
  io . fmap (Just . chomp) $ readCmd "aumix -vq|perl -pe 's/vol (\\d+).*/$1/'"
    where
      chomp :: String -> String
      chomp = takeWhile (/= '\n')

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ dzenPP { ppSep     = " | "
                            , ppTitle   = const ""
                            , ppExtras  = [ fixedWidthL AlignLeft " " 50 logTitle
                                          , fuzzyClock
                                          , volume
                                          , battery
                                          ]
                            , ppLayout  = const ""
                            , ppOutput  = hPutStrLn h
                            }
