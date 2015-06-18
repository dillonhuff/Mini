module TestUtils.Dummies.Operation(emptyOp,
                                   loadOp,
                                   vaddOp,
                                   vaddFixedSizeOp) where

import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax
import Core.SymbolTable
import TestUtils.Dummies.Loop

emptyOp =
  operation "emptyOp" emptySymtab (block [])

emptySymtab =
  miniSymtab []

loadOp =
  operation "loadOp" loadSymtab (block [load "x" "b" (iConst 0) "l1"])

loadSymtab =
  miniSymtab [("x", symInfo (sReg double) local),
              ("b", symInfo (buffer double (iConst 1)) arg)]

vaddOp =
  operation "vaddOp" vaddSymtab (block [vaddLoop])

vaddSymtab =
  miniSymtab [("i", symInfo index local),
              ("r1", symInfo (sReg double) local),
              ("r2", symInfo (sReg double) local),
              ("r3", symInfo (sReg double) local),
              ("ANRows", symInfo index arg),
              ("A", symInfo (buffer double (iVar "ANRows")) arg),
              ("B", symInfo (buffer double (iVar "ANRows")) arg),
              ("C", symInfo (buffer double (iVar "ANRows")) arg)]
  
vaddLoop =
  pS0I1ES "i" "n" vaddStmts

vaddStmts =
  [load "r1" "A" (iVar "i") "l1",
   load "r2" "B" (iVar "i") "l2",
   plus "r3" "r1" "r2" "l3",
   store "C" (iVar "i") "r3" "l4"]

vaddFixedSizeOp len =
  operation "vaddFixedSizeOp" vaddSymtab (block [pS0I1EC "i" len vaddStmts])
  
