module Syntax(toCType,
              transformBlock,
              MiniSymtab,
              localVars, arguments,
              Statement,
              transformStatementIExprs,
              Type,
              Block,
              toCBlock,
              block,
              load, store, plus, minus, times, for,
              sReg, buffer) where

import Data.List as L
import Data.Map as M

import CGen
import IndexExpression
import SymbolTable

prototype :: String -> MiniSymtab -> String
prototype n st = "void " ++ n ++ "(" ++ argumentStr st ++ ")"

argumentStr :: MiniSymtab -> String
argumentStr st = L.concat $ L.intersperse ", " $ L.map (\(n, tp) -> show tp ++ " " ++ n) $ arguments st

toCBlock :: [(String, Type)] -> Block a -> CBlock a
toCBlock decls (Block stmts) = cBlock cDecls cStmts
  where
    cDecls = L.map (\(n, tp) -> (toCType tp, n)) decls
    cStmts = L.map toCStmt $ stmts

data Block a
  = Block [Statement a]
    deriving (Eq, Ord, Show)

block = Block

transformBlock :: (Statement a -> Statement a) -> Block a -> Block a
transformBlock f (Block stmts) = block $ L.map (transformStatement f) stmts

data Statement a
  = BOp Binop String String String a
  | Load String String IExpr a
  | Store String IExpr String a
  | For String IExpr IExpr IExpr (Block a) a
    deriving (Eq, Ord, Show)

toCStmt (For n start inc end (Block bodyStatements) ann) =
  cFor (cAssign (cVar n) (iExprToCExpr start))
       (cLEQ (cVar n) (iExprToCExpr end))
       (cAssign (cVar n) (cAdd (cVar n) (iExprToCExpr inc)))
       (cBlock [] $ L.map toCStmt bodyStatements) ann
toCStmt (Load n1 n2 iExpr ann) = cExprSt (cAssign (cVar n1) (cArrAcc (cVar n2) (iExprToCExpr iExpr))) ann
toCStmt (BOp Plus s1 s2 s3 ann) = cExprSt (cAssign (cVar s1) (cAdd (cVar s2) (cVar s3))) ann
toCStmt (BOp Minus s1 s2 s3 ann) = cExprSt (cAssign (cVar s1) (cSub (cVar s2) (cVar s3))) ann
toCStmt (Store s1 iExpr s2 ann) = cExprSt (cAssign (cArrAcc (cVar s1) (iExprToCExpr iExpr)) (cVar s2)) ann

load = Load
store = Store
plus = BOp Plus
minus = BOp Minus
times = BOp Times
for n start end inc b ann = For n start end inc b ann

transformStatement :: (Statement a -> Statement a) -> Statement a -> Statement a
transformStatement f (For v s i e blk ann) = f (For v s i e (transformBlock f blk) ann)
transformStatement f s = f s

transformStatementIExprs :: (IExpr -> IExpr) -> Statement a -> Statement a
transformStatementIExprs f (For v s i e blk ann) = For v (f s) (f i) (f e) blk ann
transformStatementIExprs f s = s

data Binop
  = Plus
  | Minus
  | Times
    deriving (Eq, Ord)

instance Show Binop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"

