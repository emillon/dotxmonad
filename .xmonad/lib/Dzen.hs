{-# OPTIONS -Wall -W -Werror #-}

module Dzen(myPP) where

import XMonad.Hooks.StatusBar.PP (PP(..), dzenPP)
import XMonad.Util.Font (Align(..))
import XMonad.Util.Loggers (fixedWidthL, logTitle)

import Battery(battery)
import Clock(clock, week)

myPP :: PP
myPP =
  dzenPP { ppSep     = " | "
         , ppTitle   = const ""
         , ppExtras  = [ fixedWidthL AlignLeft " " 50 logTitle
                       , clock
                       , week
                       , battery
                       ]
         , ppLayout  = const ""
         }
