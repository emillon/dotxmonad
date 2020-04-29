{-# OPTIONS -Wall -W -Werror #-}

module Dzen(myLogHook) where

import System.IO

import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Util.Font
import XMonad.Util.Loggers

import Clock

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ dzenPP { ppSep     = " | "
                            , ppTitle   = const ""
                            , ppExtras  = [ fixedWidthL AlignLeft " " 50 logTitle
                                          , clock
                                          , week
                                          , battery
                                          ]
                            , ppLayout  = const ""
                            , ppOutput  = hPutStrLn h
                            }
