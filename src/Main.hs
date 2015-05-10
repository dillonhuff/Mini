module Main(main) where

import Data.List as L
import System.Environment

import CGen
import IndexExpression
import Lexer
import MatrixOperation
import MOpSyntax
import MiniOperation
import Parser
import RunFrontEnd
import RuntimeEvaluation
import SymbolTable
import Syntax

fileName = "/Users/dillon/Haskell/Mini/BLASLike.lspc"
cResFileName = "/Users/dillon/Haskell/Mini/BLASLike.c"

main :: IO ()
main = do
--  (fileName:cResFileName:rest) <- getArgs
  fileContents <- readFile fileName
  frontEndRes <- runFrontEnd fileName fileContents
  case frontEndRes of
    Left err -> putStrLn err
    Right res -> writeFile cResFileName (opCStrings res)

showOpCStrings opsAndTestCases =
  putStrLn $ opCStrings opsAndTestCases

opCStrings opsAndTestCases =
  "#include <stdlib.h>\n" ++ (L.concat $ L.intersperse "\n" $ L.map (\(op, _) -> matrixOpToCString op) opsAndTestCases)

matrixOpToCString matOp =
  prettyPrint 0 $ (toCFunc "") $ convertToMini $ matrixOperationToMOp matOp

