module Analysis.RegisterReduction(reduceToRegisterForm) where

import Data.List as L

import Analysis.Dependence.Register
import Core.IndexExpression
import Core.LoopTransformations
import Core.MiniSyntax

reduceToRegisterForm stmts =
  case L.and $ L.map allSimpleAccesses stmts of
    True -> Just $ convertBufferAccessesToRegisters stmts
    False -> Nothing

convertBufferAccessesToRegisters stmts =
  let uStmts = unrollLoopsBy2 stmts
      bufAccessRegisterNames = bufferAccessToRegisterNameMap uStmts in
  (bufAccessRegisterNames, L.foldl replaceBufferAccessWithRegister uStmts bufAccessRegisterNames)

bufferAccessToRegisterNameMap stmts =
  let bufAccesses = L.nub $ L.filter isBufferVal $ L.concatMap allOperands stmts in
  L.zip bufAccesses $ L.map (\i -> "$R" ++ show i) [1..(length bufAccesses)]

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
