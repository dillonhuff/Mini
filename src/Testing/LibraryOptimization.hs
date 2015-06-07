module Testing.LibraryOptimization(compileLibSpecToFileWithOptimizations) where

import Data.List as L

import BackEnd.CGen
import BackEnd.RunBackEnd
import FrontEnd.RunFrontEnd
import Core.MiniOperation

compileLibSpecToFileWithOptimizations :: [Optimization String] -> String -> String -> IO ()
compileLibSpecToFileWithOptimizations opts fileName cResFileName = do
  fileContents <- readFile fileName
  compileRes <- compileLibSpecWithOptimizations opts fileName fileContents
  case compileRes of
    Left err -> putStrLn err
    Right res -> writeFile cResFileName (opCStrings res)

opCStrings ops =
  "#include <stdlib.h>\n" ++ (L.concat $ L.intersperse "\n" $ L.map (\op -> miniOpToCString op) ops)

compileLibSpecWithOptimizations :: [Optimization String] -> String -> String -> IO (Either String [Operation String])
compileLibSpecWithOptimizations opts srcFileName libStr = do
  frontEndRes <- runFrontEnd srcFileName libStr
  case frontEndRes of
    Left err -> return $ Left err
    Right opsAndTestCases -> runBackEndWithOptimizations opts opsAndTestCases

miniOpToCString miniOp =
  prettyPrint 0 $ (toCFunc "") $ miniOp
