module TestUtils.Dummies.Statement(cA, cB,
                                   rBA, rAC, rXB,
                                   aPBQ, aPCQ, aDCB, aCBB,
                                   rCB) where

import Core.MiniSyntax

cA = loadConst "a" (doubleLit 1.0) "c1"
cB = loadConst "b" (doubleLit 2.0) "c2"

rBA = regAssign "b" "a" "r2"
rAC = regAssign "a" "c" "r3"
rXB = regAssign "x" "b" "r4"
rCB = regAssign "c" "b" "r7"

aPBQ = plus "p" "b" "q" "a5"
aPCQ = plus "p" "c" "q" "a6"
aDCB = plus "d" "c" "b" "a8"
aCBB = plus "c" "b" "b" "a9"
