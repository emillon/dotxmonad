{-# OPTIONS -Wall -W -Werror #-}

module Dzen(myLogHook) where

import Control.Monad
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

iconHomePath :: String -> String -> String
iconHomePath h name =
  icon $ h ++ "/.xmonad/icons/" ++ name ++ ".xbm"

withIcon :: String -> Logger -> Logger
withIcon iconName x = do
  xs <- x
  h  <- io getHomeDirectory
  return (prependIcon h <$> xs)
    where
      prependIcon h s = iconHomePath h iconName ++ " " ++ s

volume :: Logger
volume =
  withIcon "spkr_01" . io . fmap (Just . chomp) $ readCmd "aumix -vq|perl -pe 's/vol (\\d+).*/$1/'"
    where
      chomp :: String -> String
      chomp = takeWhile (/= '\n')

layoutIconName :: String -> Maybe String
layoutIconName "ReflectX Tall" = Just "layout_rxtall"
layoutIconName "Full"          = Just "layout_full"
layoutIconName _ = Nothing

prettyLayout :: Logger
prettyLayout = do
  ml <- logLayout
  h <- io getHomeDirectory
  return $ do
    l <- ml
    iconHomePath h <$> layoutIconName l `mplus` return l

myLogHook :: Handle -> X ()
myLogHook h =
  dynamicLogWithPP $ dzenPP { ppSep     = " | "
                            , ppTitle   = const ""
                            , ppExtras  = [ prettyLayout
                                          , fixedWidthL AlignLeft " " 50 logTitle
                                          , withIcon "clock" fuzzyClock
                                          , volume
                                          , withIcon "bat_full_02" battery
                                          ]
                            , ppLayout  = const ""
                            , ppOutput  = hPutStrLn h
                            }
