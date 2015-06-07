module Analysis.Dependence.RegisterReduction(buildDependenceGraph) where

import Data.List as L

import Analysis.Dependence.Register
import Core.IndexExpression
import Core.MiniSyntax

buildDependenceGraph stmts =
  case L.and $ L.map allSimpleAccesses stmts of
    True -> bldDepGraph stmts
    False -> Nothing

bldDepGraph stmts =
  let rrForm = reduceToRegisterForm stmts in
  registerDependenceGraph rrForm
      
reduceToRegisterForm stmts =
  convertBufferAccessesToRegisters stmts

convertBufferAccessesToRegisters stmts =
  let uStmts = unrollLoopsBy2 stmts
      bufAccessRegisterNames = bufferAccessToRegisterNameMap uStmts in
  L.foldl replaceBufferAccessWithRegister uStmts bufAccessRegisterNames

bufferAccessToRegisterNameMap stmts =
  let bufAccesses = L.filter isBufferVal $ L.concatMap allOperands stmts in
  L.zip bufAccesses $ L.map (\i -> "$R" ++ show i) stmts

replaceBufferAccessWithRegister stmts (bufAccess, reg) =
  L.map (transformStatement (compactBufferAccess bufAccess reg)) stmts

compactBufferAccess buf reg stmt =
  case isLoad stmt of
    True -> compactBufferAccessLoad buf reg stmt
    False -> case isStore stmt of
      True -> compactBufferAccessStore buf reg stmt
      False -> stmt

compactBufferAccessStore buf reg st =
  case (operandWritten st) == buf of
    True -> regAssign reg (registerName $ head $ operandsRead st) (label st)
    False -> st

compactBufferAccessLoad buf reg ld =
  case (head $ operandsRead ld) == buf of
    True -> regAssign (registerName $ operandWritten ld) reg (label ld)
    False -> ld

unrollLoopsBy2 stmts =
  L.concatMap (expandStatement unrollLoopBy2) stmts

unrollLoopBy2 stmt =
  case isFor stmt of
    True -> L.concatMap (\i -> blockStatements $ subIExprInBlock i (forInductionVariable stmt) (forBody stmt)) [iConst 0, iConst 1]
    False -> [stmt]
