{-# OPTIONS -Wall -W -Werror -fno-warn-missing-signatures #-}

module Main(main) where

import qualified Data.Map as M

import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect
import XMonad.Actions.Submap
import XMonad.Hooks.Rescreen
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.UrgencyHook
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.XSelection

import Dzen
import Layout
import Manage
import Prompt

-- Conf

main :: IO ()
main = do
  sb <- statusBarPipe pipeCmdDzen (pure myPP)
  xmonad . withUrgencyHook NoUrgencyHook . withEasySB sb defToggleStrutsKey $ myConf
    where
        pipeCmdDzen = unwords ["dzen2",
                               "-dock",
                               "-bg",
                               "black",
                               "-xs",
                               "1",
                               "-fn \"Fira Code:size=12\""
                              ]

myConf =
  addRandrChangeHook (spawn "autorandr --change") $
  (`additionalKeysP` myKeymap) $
  def { manageHook = myManageHook
      , startupHook = return () >> setWMName "LG3D" >> checkKeymap myConf myKeymap
      , layoutHook = myLayoutHook
      , workspaces = map show ([1..8] :: [Int])
      , focusedBorderColor = "#c02777"
      , normalBorderColor = "#aaaaaa"
      , modMask = mod4Mask
      , terminal = "kitty"
      , borderWidth = 2
      }


myKeymap :: [(String, X ())]
myKeymap =
  [ ("M-;", sendMessage (IncMasterN (-1)))
  , ("M-l", spawn "slock")
  , ("M-<Left>", prevWS)
  , ("M-<Right>", nextWS)
  , ("M-S-<Left>", shiftToPrev)
  , ("M-S-<Right>", shiftToNext)
  , ("<XF86AudioRaiseVolume>", volumeUp)
  , ("<XF86AudioLowerVolume>", volumeDown)
  , ("<XF86AudioPlay>", mpcToggle)
  , ("<XF86AudioPrev>", mpcPrev)
  , ("<XF86AudioNext>", mpcNext)
  , ("M-<Up>", volumeUp)
  , ("M-<Down>", volumeDown)
  , ("M-g", goToSelected gsConfig)
  , ("M-s", searchUsingMap)
  , ("M-S-s", selectSearchUsingMap)
  , ("M-m", submap mpcMap)
  , ("M-S-<Tab>", swapNextScreen)
  , ("M-v", windows copyToAll)
  , ("M-S-v", killAllOtherCopies)
  , ("M-S-u", browseToSelection)
  , ("M-<Insert>", screenshot False)
  , ("M-S-<Insert>", screenshot True)
  , ("M-r", spawnSelected gsConfig spawnableApps)
  ]

osd :: String -> X ()
osd cmd =
    spawn $ cmd ++ " | osd_cat -p middle -A right"

mpcToggle, mpcNext, mpcPrev, mpcStatus :: X ()
mpcToggle  = spawn "mpc --no-status toggle"
mpcNext    = spawn "mpc --no-status next"
mpcPrev    = spawn "mpc --no-status prev"
mpcStatus  = osd "mpc"

mpcBack, mpcForward :: String -> X ()
mpcBack    timeSpec = spawn $ "mpc seek -" ++ timeSpec
mpcForward timeSpec = spawn $ "mpc seek +" ++ timeSpec

volumeUp, volumeDown :: X ()
volumeUp   = spawn "pactl -- set-sink-volume 0 +2%"
volumeDown = spawn "pactl -- set-sink-volume 0 -2%"

mpcMap :: M.Map (KeyMask, KeySym) (X ())
mpcMap =
  M.fromList
    [ ((0, xK_t), mpcToggle)
    , ((0, xK_n), mpcNext)
    , ((0, xK_p), mpcPrev)
    , ((0, xK_s), mpcStatus)
    , ((0, xK_Left), mpcBack tenSec)
    , ((0, xK_Right), mpcForward tenSec)
    , ((0, xK_Down), mpcBack oneMin)
    , ((0, xK_Up), mpcForward oneMin)
    , ((0, xK_Page_Down), mpcBack tenMin)
    , ((0, xK_Page_Up), mpcForward tenMin)
    ]
      where
        tenSec = "00:10"
        oneMin = "01:00"
        tenMin = "10:00"

browseToSelection :: X ()
browseToSelection = do
  browser <- liftIO getBrowser
  safePromptSelection browser

gsConfig :: HasColorizer a => GSConfig a
gsConfig = def

spawnableApps :: [String]
spawnableApps =
  [ "chromium"
  , "emacs"
  , "keepassx"
  ]

screenshot :: Bool -> X ()
screenshot selection =
 spawn $ "sleep 0.2 ; scrot " ++ extraArgs selection ++ " '/tmp/%Y-%m-%d-%H:%M:%S.png' -e 'xclip -selection clipboard -t image/png < $f'"
    where
        extraArgs True = "-s "
        extraArgs False = ""
