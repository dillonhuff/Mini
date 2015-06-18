module TestUtils.Dummies.Loop(pS0I1ES,
                              p2S0I1ES) where

import Core.IndexExpression
import Core.MiniSyntax

pS0I1ES indVar end stmts = for indVar (iConst 0) (iConst 1) (iVar end) (block stmts) "loop0"

p2S0I1ES i1 e1 i2 e2 stmts =
  for i1 (iConst 0) (iConst 1) (iVar e1)
      (block [for i2 (iConst 0) (iConst 1) (iVar e2) (block stmts) "loop1Inner"])
      "loop1Outer"
