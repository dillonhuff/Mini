{-# LANGUAGE TemplateHaskell #-}

module SymbolTable(MOpSymtab,
                   mOpSymtab,
                   allLayouts,
                   mOpSymtabToMiniSymtab,
                   containsSymbol,
                   addMOpEntry,
                   subInStLayouts,
                   MOpSymInfo,
                   symScope, symEntryType, symLayout,
                   mOpSymInfo,
                   getMOpSymInfo,
                   getNumRows, getNumCols, getRowStride, getColStride, getLayout, getEntryType,
                   accessExpr,
                   Layout, layout, nr, nc, rs, cs,
                   subInLayout,
                   EntryType, doubleFloat, singleFloat,
                   MiniSymtab,
                   miniSymtab,
                   getMiniSymInfo,
                   getBufferType,
                   addEntry,
                   arguments, sReg, buffer, double, single, index,
                   localVars,
                   symInfo,
                   local, arg,
                   bufferSize, isIndex, isBuffer,
                   Type,
                   toCType) where

import Control.Lens hiding (index)
import Control.Lens.TH
import Data.List as L
import Data.Map as M

import CGen
import IndexExpression

data MOpSymtab
  = MOpSymtab (Map String MOpSymInfo)
    deriving (Eq, Ord)

instance Show MOpSymtab where
  show (MOpSymtab m) = L.concat $ L.intersperse "\n" $ L.map show $ M.toList m

mOpSymtab l = MOpSymtab $ M.fromList l

addMOpEntry string inf (MOpSymtab m) = MOpSymtab $ M.insert string inf m

containsSymbol n (MOpSymtab m) =
  case M.lookup n m of
    Just _ -> True
    _ -> False



subInStLayouts target result (MOpSymtab m) =
  MOpSymtab $ M.map (\inf -> subLayoutInfo target result inf) m

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

getEntryType n st = getMOpSymInfo n entryType st
getNumRows n st = getMOpSymInfo n numRows st
getNumCols n st = getMOpSymInfo n numCols st
getRowStride n st = getMOpSymInfo n rowStride st
getColStride n st = getMOpSymInfo n colStride st

data MOpSymInfo
  = MOpSymInfo {
    _symScope :: Scope,
    _symEntryType :: EntryType,
    _symLayout :: Layout
    } deriving (Eq, Ord, Show)

subLayoutInfo :: IExpr -> IExpr -> MOpSymInfo -> MOpSymInfo
subLayoutInfo target result (MOpSymInfo s t l) = MOpSymInfo s t $ subInLayout target result l

mOpSymInfo = MOpSymInfo

entryType (MOpSymInfo _ t _) = t
getLayout (MOpSymInfo _ _ l) = l
numRows (MOpSymInfo _ _ (Layout nr _ _ _)) = nr
numCols (MOpSymInfo _ _ (Layout _ nc _ _)) = nc
rowStride (MOpSymInfo _ _ (Layout _ _ rs _)) = rs
colStride (MOpSymInfo _ _ (Layout _ _ _ cs)) = cs

mOpSymInfoToMiniSymInfo (MOpSymInfo scope entType l) = symInfo (entryTypeToBufferType entType (layoutSizeIExpr l)) scope

data EntryType
  = DoubleFloat
  | SingleFloat
    deriving (Eq, Ord, Show)

doubleFloat = DoubleFloat
singleFloat = SingleFloat

entryTypeToBufferType DoubleFloat = Buffer DoublePrecision
entryTypeToBufferType SingleFloat = Buffer SinglePrecision

data Layout
  = Layout {
    _nr :: IExpr,
    _nc :: IExpr,
    _rs :: IExpr,
    _cs :: IExpr
    } deriving (Eq, Ord, Show)

layout nRows nCols rowStride colStride = Layout nRows nCols rowStride colStride

subInLayout target result (Layout r c rst cst) =
  Layout (subIExpr target result r) (subIExpr target result c) (subIExpr target result rst) (subIExpr target result cst)
  
layoutSizeIExpr (Layout nr nc rs cs) =
  (iAdd ind0Size (iConst 1))
  where
    ind0Size = iAdd (iMul (iAdd nr (iConst (-1))) rs) (iMul (iAdd nc (iConst (-1))) cs)

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

getMiniSymInfo :: String -> (SymbolInfo -> a) -> MiniSymtab -> a
getMiniSymInfo symName f (MiniSymtab symMap) =
  case M.lookup symName symMap of
    Just info -> f info
    Nothing -> error $ "Symbol " ++ symName ++ " not found in " ++ show symMap

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
bufferSize (SymbolInfo t _) = bufSize t

symIsArg :: SymbolInfo -> Bool
symIsArg info = isArg $ scope info

symIsIndex info = isIndex $ symType info

symIsLocalVar info = isLocalVar $ scope info

data Type
  = Buffer Type IExpr
  | Index
  | SReg Type
  | SinglePrecision
  | DoublePrecision
    deriving (Eq, Ord, Show)

toCType (Buffer pt _) = cPtr $ toCType pt
toCType Index = cInt
toCType (SReg tp) = toCType tp
toCType SinglePrecision = cFloat
toCType DoublePrecision = cDouble

sReg t = SReg t
buffer t size = Buffer t size
double = DoublePrecision
single = SinglePrecision
index = Index

bufType (Buffer t _) = t
bufSize (Buffer _ sz) = sz

isIndex Index = True
isIndex _ = False

isBuffer (Buffer _ _) = True
isBuffer _ = False

makeLenses ''Layout
makeLenses ''MOpSymInfo

allLayouts (MOpSymtab m) = L.map (\inf -> view symLayout inf) $ L.map snd $ M.toList m
