{-# OPTIONS -Wall -W -Werror #-}

module Dzen(myLogHook) where

import System.IO (Handle, hPutStrLn)

import XMonad.Core(X)
import XMonad.Hooks.DynamicLog (PP(..), dzenPP, dynamicLogWithPP)
import XMonad.Util.Font (Align(..))
import XMonad.Util.Loggers (fixedWidthL, logTitle)

import Battery(battery)
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
