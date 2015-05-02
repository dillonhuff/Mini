module Main(main) where

import CGen
import IndexExpression
import MiniOperation
import RuntimeEvaluation
import SymbolTable
import Syntax

main :: IO ()
main = do
  timeResults <- timeImplementations "" "tiny_test" (Just sanityCheckImpl) [testImpl]
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
