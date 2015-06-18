module TestUtils.Dummies.Loop(pS0I1ES, pS0I1EC, pS0ICEC, pSCICEC,
                              p2S0I1ES) where

import Core.IndexExpression
import Core.MiniSyntax

pS0I1ES indVar end stmts = for indVar (iConst 0) (iConst 1) (iVar end) (block stmts) "loop0"
pS0I1EC indVar end stmts = for indVar (iConst 0) (iConst 1) (iConst end) (block stmts) "cloop0"
pS0ICEC indVar inc end stmts = for indVar (iConst 0) (iConst inc) (iConst end) (block stmts) "cincLoop0"
pSCICEC indVar start inc end stmts = for indVar (iConst start) (iConst inc) (iConst end) (block stmts) "cccLoop0"

p2S0I1ES i1 e1 i2 e2 stmts =
  for i1 (iConst 0) (iConst 1) (iVar e1)
      (block [for i2 (iConst 0) (iConst 1) (iVar e2) (block stmts) "loop1Inner"])
      "loop1Outer"
