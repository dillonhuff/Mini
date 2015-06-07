module CopyPropagationTests(allCopyPropagationTests) where

import Core.IndexExpression
import Core.MiniSyntax
import Optimizations.CopyPropagation
import TestUtils

allCopyPropagationTests = do
  testFunction propagateTopLevelCopies topLevelCopyCases

topLevelCopyCases :: [([Statement Int], [Statement Int])]
topLevelCopyCases =
  [([], []),
   ([regAssign "a" "b" 0], []),
   ([plus "a" "b" "c" 0], [plus "a" "b" "c" 0]),
   ([regAssign "a" "b" 0, plus "a" "c" "d" 1],
    [plus "b" "c" "d" 1]),
   ([minus "c" "x" "y" 0,
     regAssign "a" "b" 1,
     for "i" (iConst 0) (iConst 1) (iVar "n") (block [plus "a" "x" "c" 2]) 3],
    [minus "c" "x" "y" 0,
     for "i" (iConst 0) (iConst 1) (iVar "n") (block [plus "b" "x" "c" 2]) 3]),
   ([regAssign "a" "b" 0,
     regAssign "c" "a" 1],
    []),
   ([regAssign "a" "b" 0,
     loadConst "a" (doubleLit 12) 1],
    [loadConst "b" (doubleLit 12) 1])]
