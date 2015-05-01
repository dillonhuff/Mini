module Syntax(Operation,
              toCFunc,
              toCType,
              Statement,
              Type,
              operation,
              getOpName,
              getOpArguments,
              getBufferSize,
              getBufferType,
              Block,
              block,
              load, store, plus, minus, times, for,
              sReg, buffer) where

import Data.List as L
import Data.Map as M

import CGen
import IndexExpression
import SymbolTable

data Operation a
  = Operation String MiniSymtab (Block a)
    deriving (Eq, Ord, Show)

toCFunc :: Operation a -> CTopLevelItem a
toCFunc op = cFuncDecl cVoid (getOpName op) cArgs cBlock
  where
    cArgs = L.map (\(n, tp) -> (toCType tp, n)) $ getOpArguments op
    cBlock = toCBlock (getOpLocalVars op) (getOpBlock op)

operation = Operation

getOpName (Operation n _ _) = n
getOpArguments (Operation _ st _) = arguments st
getOpBlock (Operation _ _ b) = b
getOpLocalVars (Operation _ st _) = localVars st

getBufferSize :: String -> Operation a -> IExpr
getBufferSize _ (Operation _ _ _) = iConst 1

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


data Binop
  = Plus
  | Minus
  | Times
    deriving (Eq, Ord)

instance Show Binop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"

