module Main(main) where

import Control.Monad
import System.Environment

import BackEnd.RunBackEnd
import Testing.LibraryOptimization

l1BLASFile = "/Users/dillon/Haskell/Mini/Level1BLAS.lspc"
cL1BLAS = "/Users/dillon/Haskell/Mini/Level1BLAS.c"

main :: IO ()
main = do
  (libPath:resultFilePath:[]) <- getArgs
  compileLibSpecToFileWithOptimizations defaultOptimizations libPath resultFilePath



