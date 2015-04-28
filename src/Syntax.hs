module Syntax(Operation,
              operation,
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

data Operation a
  = Operation String Symtab (Block a)
    deriving (Eq, Ord)

instance Show a => Show (Operation a) where
  show (Operation n st blk) = prototype n st ++ prettyPrint 0 blk

operation = Operation

prototype :: String -> Symtab -> String
prototype n st = "void " ++ n ++ "(" ++ argumentStr st ++ ")"

argumentStr :: Symtab -> String
argumentStr st = L.concat $ L.intersperse ", " $ L.map (\(n, tp) -> show tp ++ n) $ arguments st

data Symtab
  = Symtab (Map String SymbolInfo)
    deriving (Eq, Ord, Show)

symtab l = Symtab $ M.fromList l

arguments :: Symtab -> [(String, Type)]
arguments (Symtab m) =
  L.map (\(n, inf) -> (n, symType inf)) $ L.filter (\(n, inf) -> symIsArg inf) $ M.toList m

data SymbolInfo
  = SymbolInfo {
    symType :: Type,
    scope :: Scope
    } deriving (Eq, Ord, Show)

symInfo = SymbolInfo

symIsArg :: SymbolInfo -> Bool
symIsArg info = isArg $ scope info

data Type
  = Buffer PrimType
  | Index
  | SReg PrimType
    deriving (Eq, Ord, Show)

sReg t = SReg t
buffer t = Buffer t

data PrimType
  = SinglePrecision
  | DoublePrecision
    deriving (Eq, Ord, Show)

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

data Block a
  = Block [Statement a]
    deriving (Eq, Ord, Show)

instance (Show a) => Pretty (Block a) where
  prettyPrint indL (Block stmts) =
    (indent indL "{\n") ++ L.concatMap (prettyPrint (indL + 1)) stmts ++ (indent indL "\n}\n")

block = Block

data Statement a
  = BOp Binop String String String a
  | Load String String IExpr a
  | Store String IExpr String a
  | For String Int Int Int (Block a) a
    deriving (Eq, Ord, Show)

instance (Show a) => Pretty (Statement a) where
  prettyPrint indL (BOp b lhs op1 op2 ann) =
    (indent indL) (lhs ++ " = " ++ op1 ++ show b ++ op2 ++ ";\n")
  prettyPrint indL (Load lhs op1 off ann) =
    (indent indL) (lhs ++ " = " ++ op1 ++ "[" ++ show off ++ "];\n")
  prettyPrint indL (Store lhs off rhs ann) =
    (indent indL) (lhs ++ "[" ++ show off ++ "] = " ++ rhs ++ ";\n")
  prettyPrint indL (For iVar st inc end blk ann) =
    ((indent indL) ("for (" ++ iVar ++ " = " ++ (show st) ++ "; " ++ iVar ++ " <= " ++ (show end) ++ "; " ++ iVar ++ " += " ++ (show inc) ++ ") {\n")) ++
    (prettyPrint (indL + 1) blk) ++ "\n}\n"

load = Load
store = Store
plus = BOp Plus

data IExpr
  = Ind ITerm
  | Sum ITerm IExpr
    deriving (Eq, Ord, Show)

indConst i = Ind $ IConst i

data ITerm
  = ITerm Int String
  | IConst Int
    deriving (Eq, Ord, Show)

data Binop
  = Plus
  | Minus
  | Times
    deriving (Eq, Ord, Show)

class Pretty a where
  prettyPrint :: Int -> a -> String

indent :: Int -> String -> String
indent indL str = (L.replicate indL '\t') ++ str
