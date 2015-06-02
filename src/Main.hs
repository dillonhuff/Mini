module Main(main) where

import Control.Monad
import System.Environment

import RunBackEnd
import Testing.LibraryOptimization

l1BLASFile = "/Users/dillon/Haskell/Mini/Level1BLAS.lspc"
cL1BLAS = "/Users/dillon/Haskell/Mini/Level1BLAS.c"

main :: IO ()
main = compileLibSpecToFileWithOptimizations defaultOptimizations l1BLASFile cL1BLAS



