module Main(main) where

import RuntimeEvaluation
import Syntax

main :: IO ()
main = putStrLn $ show $ toCFunc sanityCheckImpl
--main = do
--  timeResults <- timeImplementations "tiny_test.c" (Just sanityCheckImpl) [sanityCheckImpl]
--  putStrLn $ show timeResults

sanityCheckImpl = operation "testOp" stSym $ block [load "b_reg" "b" (indConst 0) "",
                                                    load "c_reg" "c" (indConst 1) "",
                                                    plus "b_reg" "b_reg" "c_reg" "",
                                                    store "b" (indConst 0) "b_reg" ""]

stSym = symtab [("b_reg", symInfo (sReg double) local),
                ("c_reg", symInfo (sReg double) local),
                ("b", symInfo (buffer double) arg),
                ("c", symInfo (buffer double) arg)]
