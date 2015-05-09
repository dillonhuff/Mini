{-# LANGUAGE TemplateHaskell #-}

module MOpCodeGen(MOpCodeGen,
                  mOpCodeGen,
                  mcgMOp, mcgNextInt) where

import Control.Lens
import Control.Lens.TH

import MOpSyntax
import SymbolTable

data MOpCodeGen
  = MOpCodeGen {
    _mcgMOp :: MOp,
    _mcgNextInt :: Int
    } deriving (Eq, Ord, Show)

mOpCodeGen mp = MOpCodeGen mp 0

makeLenses ''MOpCodeGen