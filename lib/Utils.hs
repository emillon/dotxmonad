{-# OPTIONS -Wall -W -Werror #-}

module Utils where

import System.IO
import System.Process (runInteractiveCommand)

-- read a command output
readCmd :: String -> IO String
readCmd cmd =
  runInteractiveCommand cmd >>= \ (_, out, _, _) -> hGetContents out

