{-# OPTIONS -Wall -W -Werror #-}

-- Based on
-- https://chris-lamb.co.uk/posts/developing-responsive-websites-using-awesome-window-manager

module Responsive (responsiveMode, responsiveModeSelect) where

import qualified XMonad.StackSet as W

import Data.Ratio
import XMonad
import XMonad.Actions.GridSelect

data ResponsiveSize = RSExtraSmall
                    | RSSmall
                    | RSMedium
                    | RSLarge
                    | RSFullHD
    deriving (Bounded, Eq, Enum, Show)

respSize :: ResponsiveSize -> (Integer, Integer)
respSize RSExtraSmall = ( 460, 650)
respSize RSSmall      = ( 780, 750)
respSize RSMedium     = (1024, 800)
respSize RSLarge      = (1250, 850)
respSize RSFullHD     = (1280+4, 720+4)

responsiveMode :: X ()
responsiveMode =
    setResolution $ respSize RSExtraSmall

responsiveModeSelect :: GSConfig ResponsiveSize -> X ()
responsiveModeSelect conf = do
    nsm <- gridselect conf $ map (\ x -> (show x, x)) $ enumFrom minBound
    case nsm of
        Nothing -> return ()
        Just ns -> setResolution $ respSize ns

setResolution :: (Integer, Integer) -> X ()
setResolution size =
    windows $ \ ws -> case W.peek ws of
        Nothing -> ws
        Just win ->
            let sd = W.screenDetail $ W.current ws
                r = floatCenter sd size
            in
            W.float win r ws

-- Arguments are:
--   - current screen details
--   - desired window size
floatCenter :: ScreenDetail -> (Integer, Integer) -> W.RationalRect
floatCenter sd (win_w, win_h) =
    W.RationalRect x y w h
        where
            x = (1 % 2) - (w / 2)
            y = (1 % 2) - (h / 2)
            w = win_w % fi (rect_width sr)
            h = win_h % fi (rect_height sr)
            sr = screenRect sd
            fi = fromIntegral
