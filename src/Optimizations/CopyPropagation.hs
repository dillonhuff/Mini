module Optimizations.CopyPropagation(propagateAllTopLevelCopiesPossible,
                                     propagateTopLevelCopies) where

import Data.List as L

import Analysis.Basic
import Core.MiniOperation
import Core.MiniSyntax
import Core.Operand

propagateAllTopLevelCopiesPossible =
  optimization
        "PropagateAllTopLevelCopiesPossible"
        (applyToOpBlock (\b -> block $ propagateTopLevelCopies $ blockStatements b))

propagateTopLevelCopies [] = []
propagateTopLevelCopies (stmt:rest) =
  case isRegAssign stmt of
    True -> tryToPropagate stmt rest
    False -> stmt : (propagateTopLevelCopies rest)
  
tryToPropagate stmt rest =
  let b = registerName $ head $ operandsRead stmt
      a = registerName $ operandWritten stmt in
  case notReferencedIn b rest of
    True -> propagateTopLevelCopies $ L.map (transformStatement (substituteName  a b)) rest
    False -> stmt : (propagateTopLevelCopies rest)

notReferencedIn :: String -> [Statement a] -> Bool
notReferencedIn _ [] = True
notReferencedIn name stmts =
  L.and $ L.map (\stmt -> not $ referencedInStmt name stmt) stmts

referencedInStmt name stmt =
  L.or $ L.map (\n -> n == name) $ namesReferenced stmt

