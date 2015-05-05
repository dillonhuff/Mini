module RuntimeEvaluationTests() where

testGenSizeSymtab =
  miniSymtab [("a", symInfo (buffer double $ (iMul (iVar "a_nrows") (iVar "a_ncols")) arg)),
              ("b", symInfo (buffer double $ (iMul (iVar "a_nrows") (iVar "a_ncols")) arg)),
              ("a_nrows", symInfo index arg),
              ("a_ncols", symInfo index arg),
              ("a_rs", symInfo index arg),
              ("a_cs", symInfo index arg),
              ("b_rs", symInfo index arg),
              ("b_cs", symInfo index arg),
              ("a_r", symInfo (sReg double) local)]

masgBodyStmts =
  
              
