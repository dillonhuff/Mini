{-# LANGUAGE TemplateHaskell #-}

module MiniCodeGenState(MiniCodeGenState,
                        miniCodeGenState,
                        cgsMiniSymtab,
                        cgsMOpSymtab,
                        cgsStmts,
                        cgsNextInt) where

import Control.Lens
import Control.Lens.TH

import SymbolTable
import MiniSyntax

data MiniCodeGenState =
  MiniCodeGenState {
    _cgsMiniSymtab :: MiniSymtab,
    _cgsMOpSymtab :: MOpSymtab,
    _cgsStmts :: [Statement String],
    _cgsNextInt :: Int
    } deriving (Eq, Ord, Show)

miniCodeGenState = MiniCodeGenState

makeLenses ''MiniCodeGenState
