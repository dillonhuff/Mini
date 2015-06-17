module Main(main) where

import Control.Monad
import System.Environment

import BackEnd.RunBackEnd
import Testing.LibraryOptimization

libPath = "/Users/dillon/Haskell/Mini/libs/debug/dotMul.lspc"
resultFilePath = "/Users/dillon/Haskell/Mini/libs/debug/dotMul.c"

main :: IO ()
main = do
--  (libPath:resultFilePath:[]) <- getArgs
  compileLibSpecToFileWithOptimization defaultFixedSizeOperation libPath resultFilePath



