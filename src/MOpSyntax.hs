module MOpSyntax(MOp,
                 mOp,
                 madd, msub,
                 convertToMini) where

import Control.Monad.State.Lazy
import Data.Map as M

import IndexExpression
import MiniOperation
import SymbolTable
import Syntax

data MOp
  = MOp String MOpSymtab [MInstr]
    deriving (Eq, Ord, Show)

mOp name symtab instrs = MOp name symtab instrs


data MInstr
  = MBinop MBOp String String String
    deriving (Eq, Ord, Show)

madd a b c = MBinop MAdd a b c
msub a b c = MBinop MSub a b c

data MBOp
  = MAdd
  | MSub
    deriving (Eq, Ord, Show)

convertToMini :: MOp -> Operation String
convertToMini (MOp n mSt instrs) = operation n finalMiniSt $ block finalStmts
  where
    initS = initialMiniCodeGenState mSt
    finalS = execState (genMiniCode instrs) initS
    finalMiniSt = getMiniSymtab finalS
    finalStmts = miniStmts finalS

data MiniCodeGenState =
  MiniCodeGenState {
    getMiniSymtab :: MiniSymtab,
    getMOpSymtab :: MOpSymtab,
    miniStmts :: [Statement String],
    nextInt :: Int
    } deriving (Eq, Ord, Show)

getBufferTypeFromSymtab name (MiniCodeGenState s _ _ _) =
  getBufferType name s

addStatement st (MiniCodeGenState s ms sts i) = MiniCodeGenState s ms (st:sts) i

freshInt :: MiniCodeGenState -> (Int, MiniCodeGenState)
freshInt (MiniCodeGenState s ms msts i) = (i, MiniCodeGenState s ms msts (i+1))

addIndexVar varName (MiniCodeGenState s ms msts i) =
  MiniCodeGenState (addEntry varName (symInfo index local) s) ms msts i

addRegister name tp (MiniCodeGenState s ms msts i) =
  MiniCodeGenState (addEntry name (symInfo tp local) s) ms msts i

currentMOpSymtab :: State MiniCodeGenState MOpSymtab
currentMOpSymtab = do
  (MiniCodeGenState _ ms _ _) <- get
  return ms

currentMiniSymtab :: State MiniCodeGenState MiniSymtab
currentMiniSymtab = do
  (MiniCodeGenState s _ _ _) <- get
  return s

freshName :: String -> State MiniCodeGenState String
freshName prefix = do
  s <- get
  let (i, newS) = freshInt s in
    do
      put newS
      return $ prefix ++ show i

freshLabel = freshName "st_"

loadToRegister a rowInd colInd = do
  r <- freshName a
  t <- get
  l <- freshLabel
  put $ addRegister r (getBufferTypeFromSymtab a t) t
  mst <- currentMOpSymtab
  return (r, load r  a (accessExpr a rowInd colInd mst) l)

storeFromRegister bufName rowInd colInd regName = do
  l <- freshLabel
  symTab <- currentMOpSymtab
  return $ store bufName (accessExpr bufName rowInd colInd symTab) regName l
  

freshIndexVar :: State MiniCodeGenState String
freshIndexVar = do
  i <- freshName "i_"
  s <- get
  put $ addIndexVar i s
  return i

addSt :: Statement String -> State MiniCodeGenState ()
addSt st = do
  codeState <- get
  put $ addStatement st codeState

initialMiniCodeGenState :: MOpSymtab -> MiniCodeGenState
initialMiniCodeGenState mSt = MiniCodeGenState (mOpSymtabToMiniSymtab mSt) mSt [] 0

genMiniCode :: [MInstr] -> State MiniCodeGenState ()
genMiniCode [] = return ()
genMiniCode (i:is) = do
  genMiniStForInstr i
  genMiniCode is

genMiniStForInstr :: MInstr -> State MiniCodeGenState ()
genMiniStForInstr (MBinop MAdd a b c) = genMAddSt a b c
genMiniStForInstr (MBinop MSub a b c) = genMSubSt a b c

genMAddSt :: String -> String -> String -> State MiniCodeGenState ()
genMAddSt a b c = do
  maddSt <- maddStTemplate a b c
  addSt maddSt

genMSubSt :: String -> String -> String -> State MiniCodeGenState ()
genMSubSt a b c = do
  msubSt <- msubStTemplate a b c
  addSt msubSt

maddStTemplate :: String -> String -> String -> State MiniCodeGenState (Statement String)
maddStTemplate a b c = iterateOverMatTemplate a (maddBodyTemplate a b c)

msubStTemplate a b c = iterateOverMatTemplate a (msubBodyTemplate a b c)

maddBodyTemplate :: String -> String -> String -> String -> String -> State MiniCodeGenState (Block String)
maddBodyTemplate a b c rowInd colInd = do
  (aReg, lda) <- loadToRegister a rowInd colInd
  (bReg, ldb) <- loadToRegister b rowInd colInd
  stc <- storeFromRegister c rowInd colInd bReg
  l <- freshLabel
  return $ block [lda, ldb, plus bReg aReg bReg l, stc]

msubBodyTemplate a b c rowInd colInd = do
  (aReg, lda) <- loadToRegister a rowInd colInd
  (bReg, ldb) <- loadToRegister b rowInd colInd
  stc <- storeFromRegister c rowInd colInd bReg
  l <- freshLabel
  return $ block [lda, ldb, minus bReg aReg bReg l, stc]

iterateOverMatTemplate :: String -> (String -> String -> State MiniCodeGenState (Block String)) -> State MiniCodeGenState (Statement String)
iterateOverMatTemplate matName loopBodyTemplate = do
  (colInd, colLoop) <- loopOverCols matName
  (rowInd, rowLoop) <- loopOverRows matName
  body <- loopBodyTemplate rowInd colInd
  return $ rowLoop $ block [colLoop body]

loopOverCols :: String -> State MiniCodeGenState (String, Block String -> Statement String)
loopOverCols matName = do
  l <- freshLabel
  i <- freshIndexVar
  mst <- currentMOpSymtab
  let n = getNumCols matName mst in
    return (i, \body -> for i (iConst 0) (iConst 1) (iAdd n (iConst (-1))) body l)

loopOverRows :: String -> State MiniCodeGenState (String, Block String -> Statement String)
loopOverRows matName = do
  l <- freshLabel
  i <- freshIndexVar
  mst <- currentMOpSymtab
  let n = getNumRows matName mst in
    return (i, \body -> for i (iConst 0) (iConst 1) (iAdd n (iConst (-1))) body l)
  
