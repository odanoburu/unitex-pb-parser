module Main where

import UnitexPB.Parse

import System.Environment

main :: IO ()
main = do
  (a:at) <- getArgs
  case a of
    "parse" -> parseMain at
    _ -> return ()
