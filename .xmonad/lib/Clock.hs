{-# OPTIONS -Wall -W -Werror #-}

module Clock(fuzzyClock) where

import Control.Applicative
import Data.Time.Clock
import Data.Time.LocalTime

import XMonad.Core

hourSuffix :: Int -> String
hourSuffix h | h <  5 = "pile"
             | h < 10 = "cinq"
             | h < 15 = "dix"
             | h < 20 = "et quart"
             | h < 25 = "vingt"
             | h < 30 = "vingt-cinq"
             | h < 35 = "et demie"
             | h < 40 = "moins vingt-cinq"
             | h < 45 = "moins vingt"
             | h < 50 = "moins le quart"
             | h < 55 = "moins dix"
hourSuffix _          = "moins cinq"

hours :: Int -> String
hours 0 = "heure"
hours 1 = "heure"
hours _ = "heures"

say :: Int -> String
say 0 = "Minuit"
say 12 = "Midi"
say n  = cycle wordList !! n
  where
    wordList = [ "Zéro"  , "Une" , "Deux", "Trois"
               , "Quatre", "Cinq", "Six" , "Sept"
               , "Huit"  , "Neuf", "Dix" , "Onze"
               ]

fuzzy :: TimeOfDay -> String
fuzzy tod =
  say closeHour ++ " " ++ hours h ++ " " ++ hourSuffix m
    where
      h = todHour tod
      m = todMin tod
      closeHour = if m >= 35 then h + 1 else h

fuzzyClock :: X (Maybe String)
fuzzyClock =
  io $ Just <$> fuzzy <$> localTimeOfDay <$> getTime
    where
      getTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

