module Main where

import UnitexPB.Parse

import System.Environment

main :: IO ()
main = do
  (c:a:at) <- getArgs
  case c of
    "parse" -> parseMain a
    _ -> return ()
