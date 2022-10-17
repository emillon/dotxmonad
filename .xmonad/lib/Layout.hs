{-# OPTIONS -Wall -W -Werror -fno-warn-missing-signatures #-}

module Layout (myLayoutHook) where

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect

myLayoutHook = smartBorders myLayout

myLayout =
    reflectHoriz tiled ||| Full
      where
        tiled    = Tall nmaster delta ratio
        nmaster  = 1
        ratio    = 1/2
        delta    = 3/100
