module Optimizations.RegisterSynonymDeletionTests(allRegisterSynonymDeletionTests) where

import Core.MiniSyntax
import Optimizations.RegisterSynonymDeletion
import TestUtils

allRegisterSynonymDeletionTests = do
  testFunction deleteRegisterSynonymsFromStmts stmtCases

stmtCases =
  [([ldC], [ldC]),
   ([ldC, raBA], [ldC]),
   ([ldC, raBA, addBQ], [ldC, addBA])]

ldC = loadConst "a" (floatLit 1.2) "l1"
raBA = regAssign "b" "a" "l2"
raXB = regAssign "x" "b" "l4"
addBQ = plus "p" "b" "q" "l5"
addBA = plus "p" "a" "q" "l5"
