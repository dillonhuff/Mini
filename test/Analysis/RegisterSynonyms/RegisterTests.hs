module Analysis.RegisterSynonyms.RegisterTests(allRegisterSynonymsTests) where

import Data.List as L
import Data.Map as M

import Analysis.RegisterSynonyms.Register
import Core.MiniSyntax
import TestUtils

allRegisterSynonymsTests = do
  testFunction registerSynonyms regSynonymCases

regSynonymCases =
  [([ldC], M.empty)]

ldC = loadConst "a" (doubleLit 1.0) "l1"
