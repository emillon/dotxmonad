{-# OPTIONS -Wall -W -Werror #-}

module Dzen(myLogHook) where

import System.Directory
import System.IO

import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Util.Font
import XMonad.Util.Loggers

import Clock
import Utils

icon :: String -> String
icon path = "^i(" ++ path ++ ")"

withIcon :: String -> Logger -> Logger
withIcon iconName x = do
  xs <- x
  h  <- io getHomeDirectory
  return (prependIcon h <$> xs)
    where
      prependIcon h s = icon (h ++ "/.icons/" ++ iconName ++ ".xbm") ++ " " ++ s

volume :: Logger
volume =
  withIcon "spkr_01" . io . fmap (Just . chomp) $ readCmd "aumix -vq|perl -pe 's/vol (\\d+).*/$1/'"
    where
      chomp :: String -> String
      chomp = takeWhile (/= '\n')

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ dzenPP { ppSep     = " | "
                            , ppTitle   = const ""
                            , ppExtras  = [ fixedWidthL AlignLeft " " 15 logLayout
                                          , fixedWidthL AlignLeft " " 50 logTitle
                                          , withIcon "clock" fuzzyClock
                                          , volume
                                          , withIcon "bat_full_02" battery
                                          ]
                            , ppLayout  = const ""
                            , ppOutput  = hPutStrLn h
                            }
