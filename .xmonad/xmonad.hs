{-# OPTIONS -Wall -W -Werror -fno-warn-missing-signatures #-}

module Main(main) where

import qualified Data.Map as M

import Graphics.X11.ExtraTypes.XF86
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
import XMonad.Util.Scratchpad
import XMonad.Util.XSelection

import Dzen
import Layout
import Manage
import Prompt
import Responsive

-- Conf

main :: IO ()
main = do
  sb <- statusBarPipe pipeCmdDzen (pure myPP)
  xmonad . withUrgencyHook NoUrgencyHook $ myConf sb
    where
        pipeCmdDzen = unwords ["dzen2",
                               "-dock",
                               "-bg",
                               "black",
                               "-xs",
                               "1",
                               "-fn \"Fira Code:size=12\""
                              ]

myConf sb =
  withEasySB sb defToggleStrutsKey $
  addRandrChangeHook (spawn "autorandr --change") $
  def { keys = \c -> mykeys c `M.union` keys def c
      , mouseBindings = \c -> mymouse c `M.union` mouseBindings def c
      , manageHook = myManageHook
      , startupHook = setWMName "LG3D"
      , layoutHook = myLayoutHook
      , workspaces = map show ([1..8] :: [Int])
      , focusedBorderColor = "#c02777"
      , normalBorderColor = "#aaaaaa"
      , modMask = mod4Mask
      , terminal = "kitty"
      , borderWidth = 2
      , handleEventHook = handleEventHook def
      }

data MaskType = No | M | MS

toMask :: XConfig l -> MaskType -> KeyMask
toMask _ No = 0
toMask conf M = modMask conf
toMask conf MS = modMask conf .|. shiftMask

mykeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
mykeys conf = M.fromList $ map ( \ (m, k, s) -> ((toMask conf m, k), s)) $
        [ (M , xK_semicolon, sendMessage (IncMasterN (-1)))
        , (M , xK_l , spawn "slock")
        , (M , xK_twosuperior , scratchpadSpawnAction conf)
        , (M , xK_Left , prevWS)
        , (M , xK_Right , nextWS)
        , (MS, xK_Left, shiftToPrev)
        , (MS, xK_Right, shiftToNext)
        , (No, xF86XK_AudioRaiseVolume, volumeUp)
        , (No, xF86XK_AudioLowerVolume, volumeDown)
        , (No, xF86XK_AudioPlay       , mpcToggle)
        , (No, xF86XK_AudioPrev       , mpcPrev)
        , (No, xF86XK_AudioNext       , mpcNext)
        , (M , xK_Up, volumeUp)
        , (M , xK_Down, volumeDown)
        , (M , xK_g, goToSelected gsConfig)
        , (M , xK_s, searchUsingMap)
        , (MS, xK_s, selectSearchUsingMap)
        , (M , xK_m, submap mpcMap)
        , (MS, xK_Tab, swapNextScreen)
        , (M , xK_v, windows copyToAll)
        , (MS, xK_v, killAllOtherCopies)
        , (MS, xK_u, browseToSelection)
        , (M , xK_Insert, screenshot False)
        , (MS, xK_Insert, screenshot True)
        , (M , xK_r, spawnSelected gsConfig spawnableApps)
        , (M , xK_d, responsiveMode)
        , (MS, xK_d, responsiveModeSelect gsConfig)
        ]

mymouse :: XConfig l -> M.Map (ButtonMask, Button) (Window -> X ())
mymouse c = M.singleton (toMask c M, button2) (const displayClipboard)

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

displayClipboard :: X ()
displayClipboard = osd "xclip -o"

screenshot :: Bool -> X ()
screenshot selection =
 spawn $ "sleep 0.2 ; scrot " ++ extraArgs selection ++ " '/tmp/%Y-%m-%d-%H:%M:%S.png' -e 'xclip -selection clipboard -t image/png < $f'"
    where
        extraArgs True = "-s "
        extraArgs False = ""
