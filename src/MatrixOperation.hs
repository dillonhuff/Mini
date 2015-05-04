module MatrixOperation(MatrixOperation,
                       matrixOperation) where

import Text.Parsec.Pos

import SymbolTable

data MatrixOperation
  = MatrixOperation String MOpSymtab [MatrixStmt] SourcePos
    deriving (Ord, Show)

instance Eq MatrixOperation where
  (==) (MatrixOperation s1 sym1 stmts1 _) (MatrixOperation s2 sym2 stmts2 _) =
    s1 == s2 && sym1 == sym2 && stmts1 == stmts2

matrixOperation name symt stmts sp = MatrixOperation name (mOpSymtab symt) stmts sp

data MatrixStmt
  = MStmt String MExpr
    deriving (Eq, Ord, Show)

data MExpr
  = MatBinop MatBOp MExpr MExpr SourcePos
  | MatUnup MatUOp MExpr SourcePos
  | VarName String SourcePos
    deriving (Eq, Ord, Show)

varName s p = VarName s p

data MatBOp
  = MatMul
  | SMul
  | MatAdd
  | MatSub
    deriving (Eq, Ord, Show)

data MatUOp
  = MatTrans
    deriving (Eq, Ord, Show)
