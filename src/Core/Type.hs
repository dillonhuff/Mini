module Core.Type(Type,
                 single, double,
                 index,
                 isIndex,
                 sReg, buffer,
                 isBuffer, bufType, bufSize,
                 toCType) where

import BackEnd.CGen
import Core.IndexExpression

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
