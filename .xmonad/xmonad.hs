{-# OPTIONS -Wall -W -Werror -fno-warn-missing-signatures #-}

module Main(main) where

import qualified XMonad.StackSet as W
import qualified Data.Map as M

import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import Dzen
import Layout
import Manage
import Prompt

-- Conf

main :: IO ()
main = do
  hDzen <- spawnPipe pipeCmdDzen
  xmonad . withUrgencyHook NoUrgencyHook $ myConf hDzen
    where
        pipeCmdDzen = "dzen2 -xs 1 -bg black -h 16 -fn \"-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*\""

myConf h =
  XConfig { keys = \c -> mykeys c `M.union` keys defaultConfig c
          , manageHook = myManageHook
          , startupHook = setWMName "LG3D"
          , logHook = myLogHook h
          , layoutHook = myLayoutHook
          , workspaces = map show ([1..8] :: [Int])
          , focusedBorderColor = "#c02777"
          , normalBorderColor = "#aaaaaa"
          , modMask = mod4Mask 
          , terminal = uxterm False
          , borderWidth = 2
          , handleEventHook = handleEventHook defaultConfig
          , mouseBindings = mouseBindings defaultConfig
          , focusFollowsMouse = True
          }

uxterm :: Bool -> String
uxterm flt =
  "uxterm -tn xterm-256color" ++ floatStr
    where
      floatStr = if flt then " -title Float-xterm" else ""

data MaskType = No | M | MS

toMask :: XConfig l -> MaskType -> KeyMask
toMask _ No = 0
toMask conf M = modMask conf
toMask conf MS = modMask conf .|. shiftMask

mykeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
mykeys conf = M.fromList $ map ( \ (m, k, s) -> ((toMask conf m, k), s)) $
        [ (M , xK_b, sendMessage ToggleStruts)
        , (M , xK_semicolon, sendMessage (IncMasterN (-1)))
        , (M , xK_F12 , spawn "mpc --no-status pause ; xscreensaver-command -lock")
        , (M , xK_twosuperior , scratchpadSpawnAction conf)
        , (M , xK_Left , prevWS)
        , (M , xK_Right , nextWS)
        , (No, xF86XK_AudioRaiseVolume, volumeUp)
        , (No, xF86XK_AudioLowerVolume, volumeDown)
        , (No, xF86XK_AudioPlay       , mpcToggle)
        , (No, xF86XK_AudioPrev       , mpcPrev)
        , (No, xF86XK_AudioNext       , mpcNext)
        , (M , xK_Up, volumeUp)
        , (M , xK_Down, volumeDown)
        , (M , xK_g, goToSelected defaultGSConfig)
        , (M , xK_s, searchUsingMap)
        , (MS, xK_s, selectSearchUsingMap)
        , (M , xK_o, spawn $ uxterm True)
        , (M , xK_m, submap mpcMap)
        , (MS, xK_Tab, swapNextScreen)
        ] ++ [ (m, k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) azertyNumKeys
        , (f, m) <- [(W.view, M), (W.shift, MS)]
        ]

azertyNumKeys :: [KeySym]
azertyNumKeys =
  [ xK_ampersand   -- 1
  , xK_eacute      -- 2
  , xK_quotedbl    -- 3
  , xK_apostrophe  -- 4
  , xK_parenleft   -- 5
  , xK_minus       -- 6
  , xK_egrave      -- 7
  , xK_underscore  -- 8
  , xK_ccedilla    -- 9
  , xK_agrave      -- 0
  ]

mpcToggle, mpcNext, mpcPrev :: X ()
mpcToggle = spawn "mpc --no-status toggle"
mpcNext   = spawn "mpc --no-status next"
mpcPrev   = spawn "mpc --no-status prev"

volumeUp, volumeDown :: X ()
volumeUp   = spawn "amixer -c 0 sset Master 2+"
volumeDown = spawn "amixer -c 0 sset Master 2-"

mpcMap :: M.Map (KeyMask, KeySym) (X ())
mpcMap =
  M.fromList
    [ ((0, xK_t), mpcToggle)
    , ((0, xK_n), mpcNext)
    , ((0, xK_p), mpcPrev)
    ]
