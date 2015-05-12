module Main(main) where

import Control.Monad
import Data.List as L
import System.Environment

import CGen
import IndexExpression
import Lexer
import MatrixOperation
import MOpSyntax
import MiniOperation
import Parser
import RunBackEnd
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
  compileRes <- compileLibSpec fileName fileContents
  case compileRes of
    Left err -> putStrLn err
    Right res -> writeFile cResFileName (opCStrings res)

opCStrings ops =
  "#include <stdlib.h>\n" ++ (L.concat $ L.intersperse "\n" $ L.map (\op -> miniOpToCString op) ops)

compileLibSpec :: String -> String -> IO (Either String [Operation String])
compileLibSpec srcFileName libStr = do
  frontEndRes <- runFrontEnd srcFileName libStr
  return $ frontEndRes >>= runBackEnd
  
miniOpToCString miniOp =
  prettyPrint 0 $ (toCFunc "") $ miniOp

