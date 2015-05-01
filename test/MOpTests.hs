module MOpTests() where

import Data.List as L
import Data.Map as M

import IndexExpression
import MOpSyntax
import RuntimeEvaluation
import SymbolTable
import Syntax
import TestHarness

testConvertToMini = do
  testConvert "matrix_add" maddSC maddOp

testConvert :: String -> Operation String -> MOp -> IO ()
testConvert opName scImpl op =
  let resOp = convertToMini op in
  do
    rtRes <- timeImplementations "" opName (Just scImpl) [resOp]
    case L.and $ L.map (\(n, evalRes) -> passedSanityCheck evalRes) $ M.toList rtRes of
      True -> putStrLn "test passed"
      False -> putStrLn $ opName ++ " test FAILED"

maddOp =
  mOp "one_matrix_add" maddOpSym [madd "a" "b" "c"]

argLayout = layout (iConst 8) (iConst 4) (iConst 1) (iConst 8)

maddOpSym = mOpSymtab [("a", mOpSymInfo arg doubleFloat argLayout),
                       ("b", mOpSymInfo arg doubleFloat argLayout),
                       ("c", mOpSymInfo arg doubleFloat argLayout)]

maddSC =
  operation "madd_manual_sc" maddSym $
            block [] --for "i" (iConst 0) (iConst 7) (iConst 1)
{-                   (block [for "j" (iConst 0) (iConst 3) (iConst 1) $
                               (block $ load "a_r" "a" (iSum (iTerm 1 "i") (iTerm 8 "j"))
                                        load "b_r" "b" (iSum (iTerm 8 "i") (iTerm 1 "j"))
                                        load "c_r" "c" (iSum (iTerm 1 "i") (iTerm 8 "j"))
                                        plus "c_r" "a_r" "b_r"
                                        store "c" (iSum (iTerm 1 "i") (iTerm 8 "j")) "c_r"]) ""]-}

maddSym =
  miniSymtab [("a", symInfo (buffer double) arg),
              ("b", symInfo (buffer double) arg),
              ("c", symInfo (buffer double) arg),
              ("a_reg", symInfo (sReg double) local),
              ("b_reg", symInfo (sReg double) local),
              ("c_reg", symInfo (sReg double) local)]
