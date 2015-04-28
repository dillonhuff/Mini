module Syntax(Operation,
              toCFunc,
              Type,
              operation,
              getOpName,
              getOpArguments,
              symInfo,
              symtab,
              block,
              load, store, plus,
              sReg, buffer,
              single, double,
              arg, local,
              indConst) where

import Data.List as L
import Data.Map as M
import Data.Tuple as T

import CGen

data Operation a
  = Operation String Symtab (Block a)
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

prototype :: String -> Symtab -> String
prototype n st = "void " ++ n ++ "(" ++ argumentStr st ++ ")"

argumentStr :: Symtab -> String
argumentStr st = L.concat $ L.intersperse ", " $ L.map (\(n, tp) -> show tp ++ " " ++ n) $ arguments st

data Symtab
  = Symtab (Map String SymbolInfo)
    deriving (Eq, Ord, Show)

symtab l = Symtab $ M.fromList l

arguments :: Symtab -> [(String, Type)]
arguments (Symtab m) =
  L.map (\(n, inf) -> (n, symType inf)) $ L.filter (\(n, inf) -> symIsArg inf) $ M.toList m

localVars (Symtab m) =
  L.map (\(n, inf) -> (n, symType inf)) $ L.filter (\(n, inf) -> symIsLocalVar inf) $ M.toList m

data SymbolInfo
  = SymbolInfo {
    symType :: Type,
    scope :: Scope
    } deriving (Eq, Ord, Show)

symInfo = SymbolInfo

symIsArg :: SymbolInfo -> Bool
symIsArg info = isArg $ scope info

symIsLocalVar info = isLocalVar $ scope info

data Type
  = Buffer Type
  | Index
  | SReg Type
  | SinglePrecision
  | DoublePrecision
    deriving (Eq, Ord)

toCType (Buffer pt) = cPtr $ toCType pt
toCType Index = cInt
toCType (SReg tp) = toCType tp
toCType SinglePrecision = cFloat
toCType DoublePrecision = cDouble

instance Show Type where
  show (Buffer pt) = show pt ++ "*"
  show (SReg pt) = show pt

sReg t = SReg t
buffer t = Buffer t
double = DoublePrecision
single = SinglePrecision

data Scope
  = Local
  | Arg
    deriving (Eq, Ord, Show)

local = Local
arg = Arg

isArg Arg = True
isArg _ = False

isLocalVar Local = True
isLocalVar _ = False

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
  | For String Int Int Int (Block a) a
    deriving (Eq, Ord, Show)

toCStmt (Load n1 n2 iExpr ann) = cAssign (cVar n1) (cArrAcc (cVar n2) (iExprToCExpr iExpr)) ann
toCStmt (BOp Plus s1 s2 s3 ann) = cAssign (cVar s1) (cAdd (cVar s2) (cVar s3)) ann
toCStmt (Store s1 iExpr s2 ann) = cAssign (cArrAcc (cVar s1) (iExprToCExpr iExpr)) (cVar s2) ann

load = Load
store = Store
plus = BOp Plus

data IExpr
  = IConst Int
  | ITerm Int String
  | Sum IExpr IExpr
    deriving (Eq, Ord)

iExprToCExpr (IConst i) = cIntLit i
iExprToCExpr (ITerm i s) = cAdd (cIntLit i) (cVar s)

instance Show IExpr where
  show (IConst it) = show it
  show (ITerm i s) = show i ++ "*" ++ s
  show (Sum l r) = "(" ++ show l ++ " + " ++ show r ++ ")"

indConst i = IConst i

data Binop
  = Plus
  | Minus
  | Times
    deriving (Eq, Ord)

instance Show Binop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"

