module Analysis.RegisterSynonyms.RegisterTests(allRegisterSynonymsTests) where

import Data.List as L
import Data.Map as M

import Analysis.RegisterSynonyms.Register
import Core.MiniSyntax
import Core.Operand
import TestUtils.Dummies.Statement
import TestUtils.Module

allRegisterSynonymsTests = do
  testFunction registerSynonyms regSynonymCases

regSynonymCases =
  L.map (\(x, y) -> (x, M.fromList y))
  [([cA], []),
   ([cA, rBA], [(reg "b", reg "a")]),
   ([cA, rBA, rAC, aPBQ], []),
   ([cA, rBA, aPBQ], [(reg "b", reg "a")]),
   ([cA, rBA, rCB, aPCQ], [(reg "b", reg "a"), (reg "c", reg "b")])]
