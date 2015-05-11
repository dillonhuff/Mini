module MatrixOperation(MatrixOperation,
                       typeCheckMatrixOperation,
                       matAsg,
                       symProp,
                       execTypeCheck,
                       runTypeCheck,
                       dMatAsg,
                       dMatrixOperation,
                       matrixOperation,
                       matrixOperationToMOp,
                       getMatrixOpSymtab,
                       MatrixStmt,
                       matName, matrixAdd, matrixSub, matrixMul, matrixTrans, scalarMul,
                       dMatName, dMatrixAdd, dMatrixSub, dMatrixMul, dMatrixTrans, dScalarMul,
                       simplifySymtab,
                       typeCheckStmt) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.List as L
import Text.Parsec.Pos

import IndexExpression
import MOpCodeGen
import MOpSyntax
import SymbolTable
import Token

data MatrixOperation
  = MatrixOperation String MOpSymtab [MatrixStmt] SourcePos
    deriving (Ord, Show)

instance Eq MatrixOperation where
  (==) (MatrixOperation s1 sym1 stmts1 _) (MatrixOperation s2 sym2 stmts2 _) =
    s1 == s2 && sym1 == sym2 && stmts1 == stmts2

matrixOperation name symt stmts sp = MatrixOperation name (mOpSymtab symt) stmts sp

dMatrixOperation name symt stmts = matrixOperation name symt stmts dummyPos

getMatrixOpSymtab (MatrixOperation _ st _ _) = st

data MatrixStmt
  = MStmt String MExpr SourcePos
    deriving (Ord, Show)

instance Eq MatrixStmt where
  (==) (MStmt n1 e1 _) (MStmt n2 e2 _) = n1 == n2 && e1 == e2

matAsg n e p = MStmt n e p
dMatAsg n e = MStmt n e dummyPos

data MExpr
  = MatBinop MatBOp MExpr MExpr SourcePos
  | MatUnop MatUOp MExpr SourcePos
  | VarName String SourcePos
    deriving (Ord, Show)

instance Eq MExpr where
  (==) (MatBinop b1 l1 r1 _) (MatBinop b2 l2 r2 _) = b1 == b2 && l1 == l2 && r1 == r2
  (==) (MatUnop u1 n1 _) (MatUnop u2 n2 _) = u1 == u2 && n1 == n2
  (==) (VarName n1 _) (VarName n2 _) = n1 == n2

dMatrixAdd a b = MatBinop MatAdd a b dummyPos
dMatrixSub a b = MatBinop MatSub a b dummyPos
dMatrixMul a b = MatBinop MatMul a b dummyPos
dScalarMul a b = scalarMul a b dummyPos
dMatrixTrans b = MatUnop MatTrans b dummyPos
dMatName str = VarName str dummyPos

matrixMul a b p = MatBinop MatMul a b p
scalarMul a b p = MatBinop ScalMul a b p
matrixSub a b p = MatBinop MatSub a b p
matrixAdd a b p = MatBinop MatAdd a b p
matrixTrans b p = MatUnop MatTrans b p
matName s p = VarName s p

data MatBOp
  = MatMul
  | MatAdd
  | MatSub
  | ScalMul
    deriving (Eq, Ord, Show)

data MatUOp
  = MatTrans
    deriving (Eq, Ord, Show)

matrixOperationToMOp (MatrixOperation name sym stmts _) =
  let initMOp = mOpCodeGen (mOp name sym [])
      instrState = matrixStmtsToMInstrs stmts in
  view mcgMOp $ execState instrState initMOp

addInstr :: MInstr -> State MOpCodeGen ()
addInstr instr = do
  cg <- get
  put $ over (mcgMOp . mOpInstrs) (\is -> is ++ [instr]) cg
  return ()

matrixStmtsToMInstrs :: [MatrixStmt] -> State MOpCodeGen ()
matrixStmtsToMInstrs [] = return ()
matrixStmtsToMInstrs (st:stmts) = do
  matrixStToMInstrs st
  matrixStmtsToMInstrs stmts

matrixStToMInstrs :: MatrixStmt -> State MOpCodeGen ()
matrixStToMInstrs (MStmt n expr _) = do
  (eRes, eInfo) <- matrixExprToMInstrs expr
  addInstr $ masg eRes n

freshTempVar = do
  cg <- get
  let name = "tmpBuf" ++ (show $ view mcgNextInt cg) in
    do
      put $ over mcgNextInt (\n -> n+1) cg
      return name

addTmpToSymtab entType l = do
  nextName <- freshTempVar
  cg <- get
  put $ over (mcgMOp . mOpSymT) (\s -> addMOpEntry nextName (mOpSymInfo local entType l) s) cg
  return nextName

matrixExprToMInstrs :: MExpr -> State MOpCodeGen (String, MOpSymInfo)
matrixExprToMInstrs (VarName n _) = do
  cg <- get
  return (n, getMOpSymInfo n id (view (mcgMOp . mOpSymT) cg))
matrixExprToMInstrs (MatUnop MatTrans a _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  newName <- addTmpToSymtab (view symEntryType aInfo) $ transLayout (view symLayout aInfo)
  addInstr $ mtrans aName newName
  return (newName, aInfo)
matrixExprToMInstrs (MatBinop MatAdd a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  newName <- addTmpToSymtab (view symEntryType aInfo) (view symLayout aInfo)
  addInstr $ madd aName bName newName
  return (newName, aInfo)
matrixExprToMInstrs (MatBinop MatSub a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  newName <- addTmpToSymtab (view symEntryType aInfo) (view symLayout aInfo)
  addInstr $ msub aName bName newName
  return (newName, aInfo)
matrixExprToMInstrs (MatBinop ScalMul a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  newName <- addTmpToSymtab (view symEntryType bInfo) (view symLayout bInfo)
  addInstr $ msmul aName bName newName
  return (newName, aInfo)
matrixExprToMInstrs (MatBinop MatMul a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  newName <- addTmpToSymtab (view symEntryType aInfo) $ mmulLayout (view symLayout aInfo) (view symLayout bInfo)
  addInstr $ mset newName (mOpDouble 0.0)
  addInstr $ mmul aName bName newName
  return (newName, aInfo)

-- For now always generate column major results
mmulLayout l r =
  layout (view nr l) (view nc r) (iConst 1) (view nr l)

transLayout a =
  layout (view nc a) (view nr a) (iConst 1) (view nc a)

typeCheckMatrixOperation :: MatrixOperation -> Either String MatrixOperation
typeCheckMatrixOperation (MatrixOperation name symtab stmts p) = do
  newSymtab <- typeCheckStmts stmts symtab
  return $ MatrixOperation name newSymtab stmts p

typeCheckStmts :: [MatrixStmt] -> MOpSymtab -> Either String MOpSymtab
typeCheckStmts stmts st =
  foldM (\symTab stmt -> typeCheckStmt stmt symTab) st stmts

typeCheckStmt :: MatrixStmt -> MOpSymtab -> Either String MOpSymtab
typeCheckStmt stmt st = execTypeCheck (tcStmt stmt) st

type TypeCheck a = StateT a (ExceptT String Identity)

execTypeCheck a b = runIdentity $ runExceptT $ execStateT a b
runTypeCheck a b = runStateT a b

tcStmt :: MatrixStmt -> TypeCheck MOpSymtab ()
tcStmt (MStmt n e _) = do
  (l, t) <- simplifyStWithExpr e
  st <- get
  case containsSymbol n st of
    True -> do
      doAssignmentSubstitutions (getMOpSymInfo n getLayout st) l
      st2 <- get
      return ()
    False -> do
      put $ addMOpEntry n (mOpSymInfo local t l) st
      return ()

simplifySymtab :: MExpr -> MOpSymtab -> Either String MOpSymtab
simplifySymtab e st = execTypeCheck (simplifyStWithExpr e) st

symProp :: String -> (MOpSymInfo -> a) -> TypeCheck MOpSymtab a
symProp n f = do
  st <- get
  case getMOpSymInfoM n f st of
    Just p -> return p
    Nothing -> throwError $ "Symprop failed: Could not find " ++ n ++ " in " ++ show st

simplifyStWithExpr :: MExpr -> TypeCheck MOpSymtab (Layout, EntryType)
simplifyStWithExpr (VarName n _) = do
  st <- get
  layoutL <- symProp n getLayout
  entL <- symProp n entryType
  return (layoutL, entL)
simplifyStWithExpr (MatUnop u a _) = do
  (aL, t) <- simplifyStWithExpr a
  resL <- simplifyLayoutsUOp u aL
  return (resL, t)
simplifyStWithExpr (MatBinop b l r _) = do
  (leftL, _) <- simplifyStWithExpr l
  (rightL, t) <- simplifyStWithExpr r
  resL <- simplifyLayoutsBOp b leftL rightL
  return (resL, t)

simplifyLayoutsUOp :: MatUOp -> Layout -> TypeCheck MOpSymtab Layout
simplifyLayoutsUOp MatTrans l = do
  return $ layout (view nc l) (view nr l) (view cs l) (view rs l)

simplifyLayoutsBOp :: MatBOp -> Layout -> Layout -> TypeCheck MOpSymtab Layout
simplifyLayoutsBOp MatMul leftL rightL = do
  (newL, newR) <- doSubstitution (view nc leftL) (view nr rightL) leftL rightL
  return $ layout (view nr newL) (view nc newR) (view rs newL) (view cs newL)
simplifyLayoutsBOp ScalMul _ rightL = do
  return rightL
simplifyLayoutsBOp b leftL rightL = do
  (newL, newR) <- doSubstitution (view nr leftL) (view nr rightL) leftL rightL
  (resL, resR) <- doSubstitution (view nc leftL) (view nc rightL) newL newR
  return resL

doAssignmentSubstitutions :: Layout -> Layout -> TypeCheck MOpSymtab Layout
doAssignmentSubstitutions targetL resultL = do
  (newT, newR) <- doSubstitution (view nr targetL) (view nr resultL) targetL resultL
  (resT, resL) <- doSubstitution (view nc newT) (view nc newR) newT newR
  return resT

doSubstitution l r layL layR =
  case bothVarsOrEqual l r of
    True -> do
      st <- get
      put $ subInStLayouts l r st
      return $ (subInLayout l r layL, subInLayout l r layR)
    False -> throwError $ "Incompatible index values: " ++ show l ++ show r

bothVarsOrEqual l r = (isVar l && isVar r) || (l == r)
