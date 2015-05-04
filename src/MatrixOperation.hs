module MatrixOperation(MatrixOperation,
                       matAsg,
                       dMatAsg,
                       dMatrixOperation,
                       matrixOperation,
                       matrixOperationToMOp,
                       MatrixStmt,
                       matName, matrixAdd, matrixSub, matrixMul, matrixTrans,
                       dMatName, dMatrixAdd, dMatrixSub, dMatrixMul, dMatrixTrans) where

import Control.Lens
import Control.Monad
import Control.Monad.State.Lazy
import Text.Parsec.Pos

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

freshTempVar = do
  cg <- get
  let name = "tmp" ++ (show $ view mcgNextInt cg) in
    do
      put $ over mcgNextInt (+1) cg
      return name

addTmpToSymtab :: MOpSymInfo -> State MOpCodeGen String
addTmpToSymtab symInf = do
  cg <- get
  nextName <- freshTempVar
  put $ over (mcgMOp . mOpSymT) (\s -> addMOpEntry nextName symInf s) cg
  return nextName

matrixExprToMInstrs :: MExpr -> State MOpCodeGen (String, MOpSymInfo)
matrixExprToMInstrs (VarName n _) = do
  cg <- get
  return (n, getMOpSymInfo n id (view (mcgMOp . mOpSymT) cg))
matrixExprToMInstrs (MatBinop MatAdd a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  op <- get
  newName <- addTmpToSymtab aInfo
  addInstr $ madd aName bName newName
  return (newName, aInfo)
matrixExprToMInstrs (MatBinop MatSub a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  op <- get
  newName <- addTmpToSymtab aInfo
  addInstr $ msub aName bName newName
  return (newName, aInfo)

dMatrixAdd a b = MatBinop MatAdd a b dummyPos
dMatrixSub a b = MatBinop MatSub a b dummyPos
dMatrixMul a b = MatBinop MatMul a b dummyPos
dMatrixTrans b = MatUnop MatTrans b dummyPos
dMatName str = VarName str dummyPos

matrixMul a b p = MatBinop MatMul a b p
matrixSub a b p = MatBinop MatSub a b p
matrixAdd a b p = MatBinop MatAdd a b p
matrixTrans b p = MatUnop MatTrans b p
matName s p = VarName s p

data MatBOp
  = MatMul
  | MatAdd
  | MatSub
    deriving (Eq, Ord, Show)

data MatUOp
  = MatTrans
    deriving (Eq, Ord, Show)
