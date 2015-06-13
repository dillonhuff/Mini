module Optimizations.RegisterSynonymDeletionTests(allRegisterSynonymDeletionTests) where

import Core.MiniSyntax
import Optimizations.RegisterSynonymDeletion
import TestUtils

allRegisterSynonymDeletionTests = do
  testFunction deleteRegisterSynonymsFromStmts stmtCases

stmtCases =
  [([ldC], [ldC]),
   ([ldC, raBA], [ldC])]

ldC = loadConst "a" (floatLit 1.2) "l1"
raBA = regAssign "b" "a" "l2"
