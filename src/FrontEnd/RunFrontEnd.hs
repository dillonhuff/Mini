module FrontEnd.RunFrontEnd(runFrontEnd) where

import Control.Monad
import Control.Monad.Random
import Data.List as L
import Data.Map as M

import FrontEnd.Lexer
import FrontEnd.Parser
import MatrixOperation
import Testing.TestCaseGeneration

runFrontEnd :: String -> String -> IO (Either String [(MatrixOperation, [Map String Int])])
runFrontEnd fileName libStr = do
  case readLibSpec fileName libStr of
    Left err -> return $ Left err
    Right ops -> do
      liftM sequence $ sequence $ L.map (typeCheckAndGenerateTestCases lowDim highDim) ops
  
readLibSpec :: String -> String -> Either String [MatrixOperation]
readLibSpec fileName libSpecStr =
  (lexString fileName libSpecStr) >>= (parseOperation fileName)

typeCheckAndGenerateTestCases :: Int -> Int -> MatrixOperation -> IO (Either String (MatrixOperation, [Map String Int]))
typeCheckAndGenerateTestCases lo hi matOp =
  case typeCheckMatrixOperation matOp of
    Left err -> return $ Left err
    Right checkedMOp -> do
      testCases <- genTestCases lo hi (getMatrixOpSymtab checkedMOp)
      return $ Right (checkedMOp, testCases)

lowDim = 2
highDim = 100
