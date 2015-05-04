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
import RuntimeEvaluation
import SymbolTable
import Syntax

--fileName = "/Users/dillon/Haskell/Mini/ExampleLib.lspc"

main :: IO ()
main = do
  (fileName:cResFileName:rest) <- getArgs
  fileContents <- readFile fileName
  putStrLn $ libSpecToCString fileName fileContents
  writeFile cResFileName (libSpecToCString fileName fileContents)

libSpecToCString fileName libSpecStr =
  let matOps = readLibSpec fileName libSpecStr in
  L.concatMap matrixOpToCString matOps

readLibSpec fileName libSpecStr =
  let libSpecParseRes = (lexString fileName libSpecStr) >>= (parseOperation fileName) in
  case libSpecParseRes of
    Left err -> error $ show err
    Right res -> res

matrixOpToCString matOp =
  prettyPrint 0 $ toCFunc $ convertToMini $ matrixOperationToMOp matOp

