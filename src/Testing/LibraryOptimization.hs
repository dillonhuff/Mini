module Testing.LibraryOptimization(compileLibSpecToFileWithOptimization) where

import Data.List as L

import BackEnd.CGen
import BackEnd.MiniToC
import BackEnd.RunBackEnd
import FrontEnd.RunFrontEnd
import Core.MiniOperation

compileLibSpecToFileWithOptimization :: Optimization String -> String -> String -> IO ()
compileLibSpecToFileWithOptimization opt fileName cResFileName = do
  fileContents <- readFile fileName
  compileRes <- compileLibSpecWithOptimization opt fileName fileContents
  case compileRes of
    Left err -> putStrLn err
    Right res -> writeFile cResFileName (opCStrings res)

opCStrings ops =
  "#include <stdlib.h>\n" ++ (L.concat $ L.intersperse "\n" $ L.map (\op -> miniOpToCString op) ops)

compileLibSpecWithOptimization :: Optimization String -> String -> String -> IO (Either String [Operation String])
compileLibSpecWithOptimization opt srcFileName libStr = do
  frontEndRes <- runFrontEnd srcFileName libStr
  case frontEndRes of
    Left err -> return $ Left err
    Right opsAndTestCases -> runBackEndWithOptimization opt opsAndTestCases

miniOpToCString miniOp =
  prettyPrint 0 $ (toCFunc "") $ miniOp
