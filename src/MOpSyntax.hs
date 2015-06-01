{-# LANGUAGE TemplateHaskell #-}

module MOpSyntax(MOp,
                 mOp,
                 mOpName, mOpSymT, mOpInstrs,
                 addMInstr,
                 MInstr,
                 madd, msub, mtrans, mset, msmul, mmul, masg,
                 mOpFloat, mOpDouble,
                 convertToMini) where

import Control.Lens as N
import Control.Lens.TH
import Control.Monad.State.Lazy
import Data.List as L
import Data.Map as M

import IndexExpression
import MiniCodeGenState
import MiniOperation
import SymbolTable as Sym
import Syntax

data MOp
  = MOp {
    _mOpName :: String,
    _mOpSymT :: MOpSymtab,
    _mOpInstrs :: [MInstr]
    } deriving (Eq, Ord, Show)

mOp name symtab instrs = MOp name symtab instrs

addMInstr :: MInstr -> MOp -> MOp
addMInstr i (MOp n st instrs) = MOp n st (instrs ++ [i])

data MInstr
  = MBinop MBOp String String String
  | MSet String MOpLit
  | MUnop MUOp String String
    deriving (Eq, Ord, Show)

mmul a b c = MBinop MMMul a b c
madd a b c = MBinop MAdd a b c
msub a b c = MBinop MSub a b c
msmul a b c = MBinop MSMul a b c
mtrans a b = MUnop MTrans a b
masg a b = MUnop MAsg a b
mset a c = MSet a c

data MBOp
  = MAdd
  | MSub
  | MSMul
  | MMMul
    deriving (Eq, Ord, Show)

data MUOp
  = MTrans
  | MAsg
    deriving (Eq, Ord, Show)

data MOpLit
  = MOpDouble Double
  | MOpFloat Float
    deriving (Eq, Ord, Show)

mOpDouble d = MOpDouble d
mOpFloat f = MOpFloat f

mOpLitToMiniLit (MOpDouble d) = doubleLit d
mOpLitToMiniLit (MOpFloat f) = floatLit f

convertToMini :: MOp -> Operation String
convertToMini (MOp n mSt instrs) = operation n finalMiniSt $ block finalStmts
  where
    initS = initialMiniCodeGenState mSt
    finalS = execState (genMiniCode $ L.reverse instrs) initS
    finalMiniSt = view cgsMiniSymtab finalS
    finalStmts = view cgsStmts finalS

getBufferTypeFromSymtab name cgs =
  getBufferType name $ view cgsMiniSymtab cgs

addStatement st cgs = over cgsStmts (\sts -> st:sts) cgs

freshInt :: MiniCodeGenState -> (Int, MiniCodeGenState)
freshInt cgs = (view cgsNextInt cgs, over cgsNextInt (+1) cgs)

addIndexVar varName cgs =
  over cgsMiniSymtab (\st -> addEntry varName (symInfo Sym.index local) st) cgs

addRegister name tp cgs =
  over cgsMiniSymtab (\st -> addEntry name (symInfo tp local) st) cgs

currentMOpSymtab :: State MiniCodeGenState MOpSymtab
currentMOpSymtab = do
  cgs <- get
  return $ view cgsMOpSymtab cgs

currentMiniSymtab :: State MiniCodeGenState MiniSymtab
currentMiniSymtab = do
  cgs <- get
  return $ view cgsMiniSymtab cgs

freshName :: String -> State MiniCodeGenState String
freshName prefix = do
  s <- get
  let (i, newS) = freshInt s in
    do
      put newS
      return $ prefix ++ show i

freshLabel = freshName "st_"

freshRegister a rType = do
  r <- freshName a
  symtab <- get
  put $ addRegister r rType symtab
  return r

freshRegisterForBuffer a = do
  symtab <- get
  freshRegister a (getBufferTypeFromSymtab a symtab)

loadToRegister a rowInd colInd = do
  r <- freshName a
  t <- get
  l <- freshLabel
  put $ addRegister r (getBufferTypeFromSymtab a t) t
  mst <- currentMOpSymtab
  return (r, load r a (accessExpr a rowInd colInd mst) l)

loadToRegisterConst a rowVal colVal = do
  t <- get
  l <- freshLabel
  r <- freshRegister a (getBufferTypeFromSymtab a t)
  mst <- currentMOpSymtab
  return (r, load r a (accessExprConst a rowVal colVal mst) l)

loadConstToRegister c rowInd colInd = do
  l <- freshLabel
  r <- freshRegister "const" (getLitType c)
  return (r, loadConst r c l)
  
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
initialMiniCodeGenState mSt = miniCodeGenState (mOpSymtabToMiniSymtab mSt) mSt [] 0

genMiniCode :: [MInstr] -> State MiniCodeGenState ()
genMiniCode [] = return ()
genMiniCode (i:is) = do
  genMiniStForInstr i
  genMiniCode is

genMiniStForInstr :: MInstr -> State MiniCodeGenState ()
genMiniStForInstr (MBinop MMMul a b c) = genMMMulSt a b c
genMiniStForInstr (MBinop MAdd a b c) = genMAddSt a b c
genMiniStForInstr (MBinop MSub a b c) = genMSubSt a b c
genMiniStForInstr (MBinop MSMul a b c) = genMSMulSt a b c
genMiniStForInstr (MUnop MTrans a b) = genMTransSt a b
genMiniStForInstr (MUnop MAsg a b) = genMAsgSt a b
genMiniStForInstr (MSet a l) = genMSetSt a l

genMAddSt :: String -> String -> String -> State MiniCodeGenState ()
genMAddSt a b c = do
  maddSt <- maddStTemplate a b c
  addSt maddSt

genMSubSt :: String -> String -> String -> State MiniCodeGenState ()
genMSubSt a b c = do
  msubSt <- msubStTemplate a b c
  addSt msubSt

genMTransSt a b = do
  mtransSt <- mtransStTemplate a b
  addSt mtransSt

genMAsgSt a b = do
  masgSt <- masgStTemplate a b
  addSt masgSt

genMSetSt a l = do
  msetSt <- msetStTemplate a l
  addSt msetSt

genMSMulSt a b c = do
  msmulSt <- msmulStTemplate a b c
  addSt msmulSt

genMMMulSt a b c = do
  mmmulSt <- mmmulStTemplate a b c
  addSt mmmulSt

maddStTemplate :: String -> String -> String -> State MiniCodeGenState (Statement String)
maddStTemplate a b c = iterateOverMatTemplate a (maddBodyTemplate a b c)

msubStTemplate a b c = iterateOverMatTemplate a (msubBodyTemplate a b c)

mtransStTemplate a b = iterateOverMatTemplate a (mtransBodyTemplate a b)

masgStTemplate a b = iterateOverMatTemplate a (masgBodyTemplate a b)

msetStTemplate a l = iterateOverMatTemplate a (msetBodyTemplate a l)

msmulStTemplate a b c = iterateOverMatTemplate b (msmulBodyTemplate a b c)

mmmulStTemplate a b c = iterateOverMatTemplate c (mmmulBodyTemplate a b c)

maddBodyTemplate :: String -> String -> String -> String -> String -> State MiniCodeGenState (Block String)
maddBodyTemplate a b c rowInd colInd = do
  (aReg, lda) <- loadToRegister a rowInd colInd
  (bReg, ldb) <- loadToRegister b rowInd colInd
  cReg <- freshRegisterForBuffer c
  stc <- storeFromRegister c rowInd colInd cReg
  l <- freshLabel
  return $ block [lda, ldb, plus cReg aReg bReg l, stc]

msubBodyTemplate a b c rowInd colInd = do
  (aReg, lda) <- loadToRegister a rowInd colInd
  (bReg, ldb) <- loadToRegister b rowInd colInd
  cReg <- freshRegisterForBuffer c
  stc <- storeFromRegister c rowInd colInd cReg
  l <- freshLabel
  return $ block [lda, ldb, minus cReg aReg bReg l, stc]

mtransBodyTemplate a b rowInd colInd = do
  (aReg, lda) <- loadToRegister a rowInd colInd
  stb <- storeFromRegister b colInd rowInd aReg
  return $ block [lda, stb]

masgBodyTemplate a b rowInd colInd = do
  (aReg, lda) <- loadToRegister a rowInd colInd
  stb <- storeFromRegister b rowInd colInd aReg
  return $ block [lda, stb]

msetBodyTemplate a l rowInd colInd = do
  (cReg, ldc) <- loadConstToRegister (mOpLitToMiniLit l) rowInd colInd
  sta <- storeFromRegister a rowInd colInd cReg
  return $ block [ldc, sta]

msmulBodyTemplate a b c rowInd colInd = do
  (aReg, lda) <- loadToRegisterConst a 0 0
  (bReg, ldb) <- loadToRegister b rowInd colInd
  cReg <- freshRegisterForBuffer c
  stc <- storeFromRegister c rowInd colInd cReg
  l <- freshLabel
  return $ block [lda, ldb, times cReg aReg bReg l, stc]

mmmulBodyTemplate a b c rowInd colInd = do
  (kInd, aColLoop) <- loopOverCols a
  (aReg, lda) <- loadToRegister a rowInd kInd
  (bReg, ldb) <- loadToRegister b kInd colInd
  (cReg, ldc) <- loadToRegister c rowInd colInd
  dReg <- freshRegisterForBuffer c
  eReg <- freshRegisterForBuffer c
  stc <- storeFromRegister c rowInd colInd eReg
  l1 <- freshLabel
  l2 <- freshLabel
  return $ block $ [aColLoop $ block [lda, ldb, ldc, times dReg aReg bReg l1, plus eReg cReg dReg l2, stc]]

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
  

makeLenses ''MOp

