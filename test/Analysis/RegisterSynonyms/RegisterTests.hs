module Analysis.RegisterSynonyms.RegisterTests(allRegisterSynonymsTests) where

import Data.List as L
import Data.Map as M

import Analysis.RegisterSynonyms.Register
import Core.MiniSyntax
import Core.Operand
import TestUtils.Module

allRegisterSynonymsTests = do
  testFunction registerSynonyms regSynonymCases

regSynonymCases =
  L.map (\(x, y) -> (x, M.fromList y))
  [([ldC], []),
   ([ldC, raBA], [(reg "b", reg "a")]),
   ([ldC, raBA, raAC, addBQ], []),
   ([ldC, raBA, addBQ], [(reg "b", reg "a")]),
   ([ldC, raBA, raCB, addCQ], [(reg "b", reg "a"), (reg "c", reg "b")])]

ldC = loadConst "a" (doubleLit 1.0) "l1"
raBA = regAssign "b" "a" "l2"
raAC = regAssign "a" "c" "l3"
raXB = regAssign "x" "b" "l4"
addBQ = plus "p" "b" "q" "l5"
addCQ = plus "p" "c" "q" "l6"
raCB = regAssign "c" "b" "l7"
