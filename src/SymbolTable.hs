module SymbolTable(MOpSymtab,
                   mOpSymtab,
                   mOpSymtabToMiniSymtab,
                   MOpSymInfo,
                   mOpSymInfo,
                   getMOpSymInfo,
                   getNumRows, getNumCols, getRowStride, getColStride,
                   accessExpr,
                   Layout, layout,
                   EntryType, doubleFloat, singleFloat,
                   MiniSymtab,
                   miniSymtab,
                   getBufferType,
                   addEntry,
                   arguments, sReg, buffer, double, single, index,
                   localVars,
                   symInfo,
                   local, arg,
                   Type,
                   toCType) where

import Data.List as L
import Data.Map as M

import CGen
import IndexExpression

data MOpSymtab
  = MOpSymtab (Map String MOpSymInfo)
    deriving (Eq, Ord, Show)

mOpSymtab l = MOpSymtab $ M.fromList l

mOpSymtabToMiniSymtab :: MOpSymtab -> MiniSymtab
mOpSymtabToMiniSymtab (MOpSymtab symMap) =
  miniSymtab $ M.toList $ M.map mOpSymInfoToMiniSymInfo symMap

accessExpr :: String -> String -> String -> MOpSymtab -> IExpr
accessExpr symName row col (MOpSymtab symMap) =
  case M.lookup symName symMap of
    Just info -> iAdd (iMul (rowStride info) (iVar row)) (iMul (colStride info) (iVar col))
    Nothing -> error $ "Symbol " ++ symName ++ " not found in " ++ show symMap

getMOpSymInfo :: String -> (MOpSymInfo -> a) -> MOpSymtab -> a
getMOpSymInfo symName f (MOpSymtab symMap) =
  case M.lookup symName symMap of
    Just info -> f info
    Nothing -> error $ "Symbol " ++ symName ++ " not found in " ++ show symMap

getNumRows n st = getMOpSymInfo n numRows st
getNumCols n st = getMOpSymInfo n numCols st
getRowStride n st = getMOpSymInfo n rowStride st
getColStride n st = getMOpSymInfo n colStride st

data MOpSymInfo
  = MOpSymInfo Scope EntryType Layout
    deriving (Eq, Ord, Show)

mOpSymInfo = MOpSymInfo

numRows (MOpSymInfo _ _ (Layout nr _ _ _)) = nr
numCols (MOpSymInfo _ _ (Layout _ nc _ _)) = nc
rowStride (MOpSymInfo _ _ (Layout _ _ rs _)) = rs
colStride (MOpSymInfo _ _ (Layout _ _ _ cs)) = cs

mOpSymInfoToMiniSymInfo (MOpSymInfo scope entType _) = symInfo (entryTypeToBufferType entType) scope

data EntryType
  = DoubleFloat
  | SingleFloat
    deriving (Eq, Ord, Show)

doubleFloat = DoubleFloat
singleFloat = SingleFloat

entryTypeToBufferType DoubleFloat = Buffer DoublePrecision
entryTypeToBufferType SingleFloat = Buffer SinglePrecision

data Layout
  = Layout IExpr IExpr IExpr IExpr
    deriving (Eq, Ord, Show)

layout nRows nCols rowStride colStride = Layout nRows nCols rowStride colStride

data Type
  = Buffer Type
  | Index
  | SReg Type
  | SinglePrecision
  | DoublePrecision
    deriving (Eq, Ord, Show)

toCType (Buffer pt) = cPtr $ toCType pt
toCType Index = cInt
toCType (SReg tp) = toCType tp
toCType SinglePrecision = cFloat
toCType DoublePrecision = cDouble

sReg t = SReg t
buffer t = Buffer t
double = DoublePrecision
single = SinglePrecision
index = Index

bufType (Buffer t) = t

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

data MiniSymtab
  = MiniSymtab (Map String SymbolInfo)
    deriving (Eq, Ord, Show)

miniSymtab l = MiniSymtab $ M.fromList l

addEntry name info (MiniSymtab m) = MiniSymtab $ M.insert name info m

getBufferType :: String -> MiniSymtab -> Type
getBufferType n (MiniSymtab symMap) =
  case M.lookup n symMap of
    Just info -> bufferType info
    Nothing -> error $ "Could not find symbol " ++ n ++ " in " ++ show symMap

arguments :: MiniSymtab -> [(String, Type)]
arguments (MiniSymtab m) =
  L.map (\(n, inf) -> (n, symType inf)) $ L.filter (\(n, inf) -> symIsArg inf) $ M.toList m

localVars (MiniSymtab m) =
  L.map (\(n, inf) -> (n, symType inf)) $ L.filter (\(n, inf) -> symIsLocalVar inf) $ M.toList m

data SymbolInfo
  = SymbolInfo {
    symType :: Type,
    scope :: Scope
    } deriving (Eq, Ord, Show)

symInfo = SymbolInfo

bufferType (SymbolInfo t _) = bufType t

symIsArg :: SymbolInfo -> Bool
symIsArg info = isArg $ scope info

symIsLocalVar info = isLocalVar $ scope info
