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

fileName = "/Users/dillon/Haskell/Mini/ExampleLib.lspc"

main :: IO ()
main = do
--  (fileName:rest) <- getArgs
  fileContents <- readFile fileName
  putStrLn $ libSpecToCString fileName fileContents

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

{-  timeResults <- timeImplementations "" "tiny_test" (Just sanityCheckImpl) [testImpl]
  putStrLn $ show timeResults

sanityCheckImpl = operation "sanity_check" stSym $ block [load "b_reg" "b" (iConst 0) "",
                                                    load "c_reg" "c" (iConst 1) "",
                                                    plus "b_reg" "b_reg" "c_reg" "",
                                                    store "b" (iConst 0) "b_reg" ""]

testImpl = operation "testOp" stSym $ block [load "c_reg" "c" (iConst 1) "",
                                                    load "b_reg" "b" (iConst 0) "",
                                                    plus "b_reg" "b_reg" "c_reg" "",
                                                    store "b" (iConst 0) "b_reg" ""]

stSym = miniSymtab [("b_reg", symInfo (sReg double) local),
                ("c_reg", symInfo (sReg double) local),
                ("b", symInfo (buffer double (iConst 4)) arg),
                ("c", symInfo (buffer double (iConst 4)) arg)]
-}
