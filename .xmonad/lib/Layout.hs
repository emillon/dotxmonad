{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -Wall -W -Werror -fno-warn-missing-signatures #-}

module Layout (myLayoutHook) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed

myLayoutHook = smartBorders . avoidStruts $ myLayout

myTabbed = tabbed shrinkText myTheme
  where
    myTheme = defaultTheme {   activeColor = "black"
                           , inactiveColor = "black"
                           }


myLayout =
  onWorkspace "2"  (myTabbed ||| Full) $
  {-onWorkspace "8"  (withIM (1/5) (Role "buddy_list") Full ||| Full) $-}
    reflectHoriz tiled ||| Full
      where
        tiled    = Tall nmaster delta ratio
        nmaster  = 1
        ratio    = 1/2
        delta    = 3/100
