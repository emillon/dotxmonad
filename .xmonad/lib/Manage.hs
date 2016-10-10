{-# OPTIONS -Wall -W -Werror #-}

module Manage(myManageHook) where

import qualified XMonad.StackSet as W

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Workspace numbers

data WorkspaceName = Web
workspace :: WorkspaceName -> String
workspace Web  = "2"

manageFloat :: ManageHook
manageFloat =
  composeAll
    [ isFullscreen                          --> doFullFloat
    , appName   =? "sun-awt-X11-XFramePeer" --> doFloat
    , appName   =? "DOTTY"                  --> doCenterFloat
    , className =? "Xmessage"               --> doCenterFloat
    , className =? "Skype"                  --> doFloat
    , appName   =? "Dialog"                 --> doFloat
    , title     =? "Float-xterm"            --> doCenterFloat
    , className =? "xli"                    --> doFloat
    , isIdaTextPrompt --> doRectFloat (W.RationalRect 0.75 0.20 0.25 0.60)
    , title =? "qiv" --> doFloat
    ]

isIdaTextPrompt :: Query Bool
isIdaTextPrompt =
  title =? "Please enter text" <&&> (className =? "Idaq" <||> className =? "Idaq64")

manageShift :: ManageHook
manageShift = 
  composeOne (transience:shifts)
    where
      shifts = map (\ (c, wn) -> className =? c -?> doF (W.shift $ workspace wn)) rules
      rules  = [("Iceweasel", Web )
               ,("Uzbl-core", Web )
               ,("Chromium", Web )
               ]

myManageHook :: ManageHook
myManageHook =
  mconcat [ manageFloat
          , manageShift
          , manageHook def
          , manageDocks
          ]
