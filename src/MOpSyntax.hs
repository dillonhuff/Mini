module MOpSyntax(MOp,
                 mOp,
                 madd,
                 mSymtab,
                 convertToMini) where

import Data.Map as M

import Syntax

data MOp a
  = MOp String MSymtab [MInstr a]
    deriving (Eq, Ord, Show)

mOp name symtab instrs = MOp name symtab instrs

data MSymtab
  = MSymtab (Map String MSymInfo)
    deriving (Eq, Ord, Show)

mSymtab = MSymtab

data MInstr a
  = MAdd String String String a
    deriving (Eq, Ord, Show)

madd a b c ann = MAdd a b c ann

data MSymInfo
  = MSymInfo
    deriving (Eq, Ord, Show)

convertToMini :: MOp a -> Operation a
convertToMini op = error "convertToMini not implemented"
