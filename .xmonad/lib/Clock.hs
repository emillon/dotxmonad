{-# OPTIONS -Wall -W -Werror #-}

module Clock(clock, week) where

import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

import XMonad.Core

getTime :: IO LocalTime
getTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

clock :: X (Maybe String)
clock = io $ do
    t <- getTime
    let l = localTimeOfDay t
        time = formatTime defaultTimeLocale "%R" l
    return $ Just time

week :: X (Maybe String)
week = io $ do
    t <- getTime
    let day = localDay t
        (_, w, _) = toWeekDate day
    return $ Just $ "W" ++ show w
