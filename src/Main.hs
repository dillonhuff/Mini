module Main(main) where

import Control.Monad
import System.Environment

import BackEnd.RunBackEnd
import Testing.LibraryOptimization

libPath = "/Users/dillon/Haskell/Mini/libs/SmallFixedSizes/Lvl1ish.lspc"
resultFilePath = "/Users/dillon/Haskell/Mini/libs/SmallFixedSizes/Lvl1ish.c"

main :: IO ()
main = do
--  (libPath:resultFilePath:[]) <- getArgs
  compileLibSpecToFileWithOptimization defaultOptimization libPath resultFilePath



