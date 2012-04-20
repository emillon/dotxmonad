{-# OPTIONS -Wall -W -Werror #-}

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
        myConf hd = defaultConfig
                      { keys               = \c -> mykeys c `M.union` keys defaultConfig c
                      , manageHook         = myManageHook
                      , startupHook        = setWMName "LG3D"
                      , logHook            = myLogHook hd
                      , layoutHook         = myLayoutHook
                      , workspaces         = map show ([1..8] :: [Int])
--                    , focusedBorderColor = "#729fcf"
--                    , focusedBorderColor = "#aa0000"
                      , focusedBorderColor = "#c02777"
                      , normalBorderColor  = "#aaaaaa"
                      , modMask            = mask
                      , terminal           = myTerm
                      , borderWidth        = 2
                      }
        mask = mod4Mask 
        {-myTerm = "urxvt -tn rxvt-256color"-}
        myTerm = "uxterm -tn xterm-256color"

        pipeCmdDzen = "dzen2 -xs 1 -bg black -h 16 -fn \"-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*\""

        mykeys conf = M.fromList $
                [((mask, xK_b), sendMessage ToggleStruts)
                ,((mask, xK_semicolon), sendMessage (IncMasterN (-1)))
                ,((mask, xK_F12 ), spawn "mpc --no-status pause ; xscreensaver-command -lock")
                ,((mask, xK_twosuperior ), scratchpadSpawnAction conf)
                ,((mask, xK_Left ), prevWS)
                ,((mask, xK_Right ), nextWS)
                ,((0   , xF86XK_AudioRaiseVolume), volumeUp)
                ,((0   , xF86XK_AudioLowerVolume), volumeDown)
--              ,((0   , xF86XK_AudioMute)       , spawn "amixer -c 0 set  Master toggle")
                ,((0   , xF86XK_AudioPlay)       , mpcToggle)
                ,((0   , xF86XK_AudioPrev)       , mpcPrev)
                ,((0   , xF86XK_AudioNext)       , mpcNext)
                ,((mask, xK_Up), volumeUp)
                ,((mask, xK_Down), volumeDown)
                ,((mask, xK_g), goToSelected defaultGSConfig)
                ,((mask, xK_s), searchUsingMap)
                ,((mask .|. shiftMask, xK_s), selectSearchUsingMap)
                ,((mask, xK_o), spawn $ myTerm ++ " -title Float-xterm")
                ,((mask, xK_m), submap mpcMap)
                ,((mask .|. shiftMask, xK_Tab), swapNextScreen)
                ]
                ++
                [((m .|. mask, k), windows $ f i)
                    | (i, k) <- zip (XMonad.workspaces conf)
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
                           ],
                    (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

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
