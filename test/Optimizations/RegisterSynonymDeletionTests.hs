module Optimizations.RegisterSynonymDeletionTests(allRegisterSynonymDeletionTests) where

import Core.MiniSyntax
import Optimizations.RegisterSynonymDeletion
import TestUtils

allRegisterSynonymDeletionTests = do
  testFunction deleteRegisterSynonymsFromStmts stmtCases

stmtCases =
  [([ldC], [ldC]),
   ([ldC, raBA], [ldC]),
   ([ldC, raBA, addBQ], [ldC, addBA]),
   ([ldC, raBA, raCB, addCQ], [ldC, addAQ])]

ldC = loadConst "a" (floatLit 1.2) "l1"
raBA = regAssign "b" "a" "l2"
raXB = regAssign "x" "b" "l4"
addBQ = plus "p" "b" "q" "l5"
addBA = plus "p" "a" "q" "l5"
addCQ = plus "p" "c" "q" "l6"
addAQ = plus "p" "a" "q" "l6"
raCB = regAssign "c" "b" "l7"

