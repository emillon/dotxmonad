{-# OPTIONS -Wall -W -Werror #-}

module Battery(battery) where

import XMonad(liftIO)
import XMonad.Core(X)
import XMonad.Hooks.DynamicLog (dzenColor, trim)
import XMonad.Util.Loggers (Logger)

data BatteryLevel
    = Critical
    | Low
    | OK

batteryLevel :: Int -> BatteryLevel
batteryLevel n | n < 10 = Critical
batteryLevel n | n < 20 = Low
batteryLevel _          = OK

styleBatteryLevel :: BatteryLevel -> String -> String
styleBatteryLevel Critical = dzenColor "black" "red"
styleBatteryLevel Low = dzenColor "black" "yellow"
styleBatteryLevel OK = id

xReadFile :: String -> X String
xReadFile path = liftIO $ trim <$> readFile path

data Status
  = Discharging
  | Charging
  | Unknown
  | Full

parseStatus :: String -> Either String Status
parseStatus "Discharging" = return Discharging
parseStatus "Charging" = return Charging
parseStatus "Unknown" = return Unknown
parseStatus "Full" = return Full
parseStatus s = Left s

logUnknown :: (a -> String) -> Either String a -> String
logUnknown l (Right x) = l x
logUnknown _ (Left e) = "?(" ++ e ++ ")"

logStatus :: Status -> String
logStatus Discharging = "-"
logStatus Charging = "+"
logStatus Unknown = "?"
logStatus Full = ""

data BatteryInfo =
  BatteryInfo
    { batteryCapacity :: Int
    , batteryStatus :: Either String Status
    }

getBatteryInfo :: String -> X BatteryInfo
getBatteryInfo dir = do
  let capacityFile = dir ++ "/capacity"
      statusFile = dir ++ "/status"
  capacityString <- xReadFile capacityFile
  statusString <- xReadFile statusFile
  let capacity = read capacityString
      status = parseStatus statusString
  return $ BatteryInfo capacity status

logBatteryInfo :: BatteryInfo -> String
logBatteryInfo info =
  styleBatteryLevel level (show capacity ++ "%") ++ logUnknown logStatus status
    where
      capacity = batteryCapacity info
      level = batteryLevel capacity
      status = batteryStatus info

battery :: Logger
battery =
  Just . logBatteryInfo <$> getBatteryInfo dir
    where
      dir = "/sys/class/power_supply/BAT0"
