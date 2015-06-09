module Analysis.Dependence.RegisterReduction(buildDependenceGraph,
                                             reduceToRegisterForm) where

import Data.List as L

import Analysis.Dependence.Graph
import Analysis.Dependence.Register
import Core.IndexExpression
import Core.MiniSyntax

buildDependenceGraph :: (Ord a, Show a) => [Statement a] -> Maybe (DependenceGraph a)
buildDependenceGraph stmts = do
  bufMapRRForm <- reduceToRegisterForm stmts
  registerDependenceGraph $ snd $ bufMapRRForm

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

unrollLoopsBy2 stmts =
  transformStatementList (L.concatMap (expandStatement unrollLoopBy2)) stmts

unrollLoopBy2 stmt =
  case isFor stmt of
    True -> L.concatMap (\i -> blockStatements $ subIExprInBlock i (forInductionVariable stmt) (forBody stmt)) [iConst 0, iConst 1]
    False -> [stmt]
