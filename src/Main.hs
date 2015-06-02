module Main(main) where

import Control.Monad
import Data.List as L
import System.Environment

import CGen
import MiniOperation
import RunBackEnd
import FrontEnd.RunFrontEnd

fileName = "/Users/dillon/Haskell/Mini/Level1BLAS.lspc"
cResFileName = "/Users/dillon/Haskell/Mini/Level1BLAS.c"

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
  case frontEndRes of
    Left err -> return $ Left err
    Right opsAndTestCases -> runBackEnd opsAndTestCases
  
miniOpToCString miniOp =
  prettyPrint 0 $ (toCFunc "") $ miniOp

