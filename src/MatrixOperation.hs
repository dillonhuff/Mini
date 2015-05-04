module MatrixOperation(MatrixOperation,
                       matAsg,
                       dMatAsg,
                       matrixOperation,
                       matName, matrixAdd,
                       dMatName,
                       dMatrixAdd) where

import Text.Parsec.Pos

import SymbolTable
import Token

data MatrixOperation
  = MatrixOperation String MOpSymtab [MatrixStmt] SourcePos
    deriving (Ord, Show)

instance Eq MatrixOperation where
  (==) (MatrixOperation s1 sym1 stmts1 _) (MatrixOperation s2 sym2 stmts2 _) =
    s1 == s2 && sym1 == sym2 && stmts1 == stmts2

matrixOperation name symt stmts sp = MatrixOperation name (mOpSymtab symt) stmts sp

data MatrixStmt
  = MStmt String MExpr SourcePos
    deriving (Ord, Show)

instance Eq MatrixStmt where
  (==) (MStmt n1 e1 _) (MStmt n2 e2 _) = n1 == n2 && e1 == e2

matAsg n e p = MStmt n e p
dMatAsg n e = MStmt n e dummyPos

data MExpr
  = MatBinop MatBOp MExpr MExpr SourcePos
  | MatUnup MatUOp MExpr SourcePos
  | VarName String SourcePos
    deriving (Ord, Show)

instance Eq MExpr where
  (==) (MatBinop b1 l1 r1 _) (MatBinop b2 l2 r2 _) = b1 == b2 && l1 == l2 && r1 == r2
  (==) (VarName n1 _) (VarName n2 _) = n1 == n2

dMatName str = VarName str dummyPos
dMatrixAdd a b = MatBinop MatAdd a b dummyPos

matrixAdd a b p = MatBinop MatAdd a b p
matName s p = VarName s p

data MatBOp
  = MatMul
  | SMul
  | MatAdd
  | MatSub
    deriving (Eq, Ord, Show)

data MatUOp
  = MatTrans
    deriving (Eq, Ord, Show)
