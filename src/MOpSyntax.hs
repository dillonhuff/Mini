module MOpSyntax(MOp,
                 mOp,
                 madd,
                 mSymtab,
                 convertToMini) where

import Control.Monad.State.Lazy
import Data.Map as M

import Syntax

data MOp
  = MOp String MSymtab [MInstr]
    deriving (Eq, Ord, Show)

mOp name symtab instrs = MOp name symtab instrs

data MSymtab
  = MSymtab (Map String MSymInfo)
    deriving (Eq, Ord, Show)

mSymtab = MSymtab

data MInstr
  = MAdd String String String
    deriving (Eq, Ord, Show)

madd a b c = MAdd a b c

data MSymInfo
  = MSymInfo
    deriving (Eq, Ord, Show)

convertToMini :: MOp -> Operation String
convertToMini (MOp n mSt instrs) = operation n finalMiniSt $ block finalStmts
  where
    initS = initialMiniCodeGenState n mSt instrs
    finalS = execState (genMiniCode instrs) initS
    finalMiniSt = miniSymtab finalS
    finalStmts = miniStmts finalS

data MiniCodeGenState =
  MiniCodeGenState {
    miniSymtab :: Symtab,
    mOpSymtab :: MSymtab,
    miniStmts :: [Statement String]
    } deriving (Eq, Ord, Show)

addStatement st (MiniCodeGenState s ms sts) = MiniCodeGenState s ms (st:sts)

addSt :: Statement String -> State MiniCodeGenState ()
addSt st = do
  codeState <- get
  put $ addStatement st codeState

initialMiniCodeGenState :: String -> MSymtab -> [MInstr] -> MiniCodeGenState
initialMiniCodeGenState n mSt instrs = error "miniCodeGenState not implemented"

genMiniCode :: [MInstr] -> State MiniCodeGenState ()
genMiniCode [] = return ()
genMiniCode (i:is) = do
  genMiniStForInstr i
  genMiniCode is

genMiniStForInstr :: MInstr -> State MiniCodeGenState ()
genMiniStForInstr (MAdd a b c) = genMAddSt a b c

genMAddSt :: String -> String -> String -> State MiniCodeGenState ()
genMAddSt a b c = do
  maddSt <- maddStTemplate a b c
  addSt maddSt

maddStTemplate :: String -> String -> String -> State MiniCodeGenState (Statement String)
maddStTemplate a b c = do
  body <- maddBodyTemplate a b c
  iterateOverMatTemplate a body

maddBodyTemplate :: String -> String -> String -> State MiniCodeGenState (Block String)
maddBodyTemplate a b c = error "maddBodyTemplate not implemented"

iterateOverMatTemplate :: String -> Block String -> State MiniCodeGenState (Statement String)
iterateOverMatTemplate matName loopBody = error "iterateOverMatTemplate not implemented"{-do
  rowVar <- freshIndVar
  rowStart <- rowStartVal matName
  rowEnd <- rowEndVal matName
  colVar <- freshIndVar
  colStart <- colStartVal matName
  colEnd <- colEndVal matName-}
  
