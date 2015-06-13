module Optimizations.RegisterSynonymDeletion(deleteRegisterSynonyms,
                                             deleteRegisterSynonymsFromStmts) where

import Data.List as L
import Data.Map as M

import Analysis.RegisterReduction
import Analysis.RegisterSynonyms.Register
import Core.LoopTransformations
import Core.MiniOperation
import Core.MiniSyntax
import Utils.MapUtils

deleteRegisterSynonyms :: (Ord a, Show a) => Optimization a
deleteRegisterSynonyms =
  optimization
        "DeleteRegisterSynonyms"
        (applyToOpBlock (\b -> block $ deleteRegisterSynonymsFromStmts $ blockStatements b))

deleteRegisterSynonymsFromStmts :: (Ord a, Show a) => [Statement a] -> [Statement a]
deleteRegisterSynonymsFromStmts stmts =
--  case reduceToRegisterForm stmts of
--    Just (_, rrForm) ->
  let regsToDelete = registerSynonyms $ unrollLoopsBy2 stmts in
  deleteSynonyms regsToDelete stmts
--    Nothing -> stmts

deleteSynonyms :: Map Operand Operand -> [Statement a] -> [Statement a]
deleteSynonyms regsToDelete stmts =
  case M.null regsToDelete of
    True -> stmts
    False ->
      let r = pickSynonymReg regsToDelete
          newRegsToDelete = M.delete r regsToDelete
          newStmts = deleteReg r (lookupF r regsToDelete) stmts in
      deleteSynonyms newRegsToDelete newStmts

pickSynonymReg regsToDelete =
  let toDel = M.keys regsToDelete
      synonyms = M.elems regsToDelete in
  L.head $ toDel L.\\ synonyms

deleteReg target result stmts =
  let noAssignsToTarget = transformStatementList (L.filter (\st -> not $ isRegAssign st && operandWritten st == target)) stmts
      newStmts = transformStatementList (L.map $ substituteName (registerName target) (registerName result)) noAssignsToTarget in
  newStmts
