{-# LANGUAGE TemplateHaskell #-}

module Core.MiniCodeGenState(MiniCodeGenState,
                        miniCodeGenState,
                        cgsMiniSymtab,
                        cgsMOpSymtab,
                        cgsStmts,
                        cgsNextInt) where

import Control.Lens
import Control.Lens.TH

import Core.SymbolTable
import Core.MiniSyntax

data MiniCodeGenState =
  MiniCodeGenState {
    _cgsMiniSymtab :: MiniSymtab,
    _cgsMOpSymtab :: MOpSymtab,
    _cgsStmts :: [Statement String],
    _cgsNextInt :: Int
    } deriving (Eq, Ord, Show)

miniCodeGenState = MiniCodeGenState

makeLenses ''MiniCodeGenState
