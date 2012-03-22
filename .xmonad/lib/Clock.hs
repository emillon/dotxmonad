{-# OPTIONS -Wall -W -Werror #-}

module Clock(fuzzyClock, fzcTest) where

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

hours :: Int -> [String]
hours 1 = ["heure"]
hours 13 = ["heure"]
hours 12 = []
hours 0 = []
hours _ = ["heures"]

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
fuzzy (TimeOfDay h0 m _) =
  unwords $ [say h] ++ hours h ++ [hourSuffix m]
    where
      h = if m >= 35 then (h0 + 1) `mod` 24 else h0

fuzzyClock :: X (Maybe String)
fuzzyClock =
  io $ Just <$> fuzzy <$> localTimeOfDay <$> getTime
    where
      getTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

fzcTest :: Bool
fzcTest =
  all ok tests
    where
      ok ((h, m), s) = fuzzy (TimeOfDay h m 0) == s
      tests = [ (( 3, 14), "Trois heures dix")
              , ((13,  0), "Une heure pile")
              , ((12,  0), "Midi pile")
              , (( 0,  5), "Minuit cinq")
              , (( 2, 45), "Trois heures moins le quart")
              , ((23, 40), "Minuit moins vingt")
              ]
