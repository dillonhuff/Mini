module MatrixOperation(MatrixOperation,
                       matAsg,
                       dMatAsg,
                       dMatrixOperation,
                       matrixOperation,
                       matrixOperationToMOp,
                       MatrixStmt,
                       matName, matrixAdd, matrixSub, matrixMul, matrixTrans,
                       dMatName, dMatrixAdd, dMatrixSub, dMatrixMul, dMatrixTrans) where

import Control.Monad
import Control.Monad.State.Lazy
import Text.Parsec.Pos

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
  let initMOp = mOp name sym []
      instrState = matrixStmtsToMInstrs stmts in
  execState instrState initMOp

addInstr :: MInstr -> State MOp ()
addInstr instr = do
  op <- get
  put $ addMInstr instr op
  return ()

matrixStmtsToMInstrs :: [MatrixStmt] -> State MOp ()
matrixStmtsToMInstrs [] = return ()
matrixStmtsToMInstrs (st:stmts) = do
  matrixStToMInstrs st
  matrixStmtsToMInstrs stmts

matrixStToMInstrs :: MatrixStmt -> State MOp ()
matrixStToMInstrs (MStmt n expr _) = do
  (eRes, eInfo) <- matrixExprToMInstrs expr
  addInstr $ masg n eRes

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

addTmpToSymtab :: MOpSymInfo -> State MOp String
addTmpToSymtab symInf = error "addTmpToSymtab not implemented yet"

matrixExprToMInstrs :: MExpr -> State MOp (String, MOpSymInfo)
matrixExprToMInstrs (MatBinop MatAdd a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  op <- get
  newName <- addTmpToSymtab aInfo
  addInstr $ madd aName bName newName
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
