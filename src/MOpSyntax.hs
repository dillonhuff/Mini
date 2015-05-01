module MOpSyntax(MOp,
                 mOp,
                 madd,
                 convertToMini) where

import Control.Monad.State.Lazy
import Data.Map as M

import IndexExpression
import SymbolTable
import Syntax

data MOp
  = MOp String MOpSymtab [MInstr]
    deriving (Eq, Ord, Show)

mOp name symtab instrs = MOp name symtab instrs


data MInstr
  = MAdd String String String
    deriving (Eq, Ord, Show)

madd a b c = MAdd a b c

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
  return (r, load r  a (iConst 0) l)

storeFromRegister bufName rowInd colInd regName = do
  rs <- getRowStride bufName
  cs <- getColStride bufName
  l <- freshLabel
  return $ store bufName (iAdd (iMul (iConst 1) (iConst 1)) (iMul (iConst 1) (iConst 1))) regName l
  

freshIndexVar :: State MiniCodeGenState String
freshIndexVar = do
  i <- freshName "i_"
  s <- get
  put $ addIndexVar i s
  return i

getNumRows :: String -> State MiniCodeGenState IExpr
getNumRows n = return $ iConst 1

getNumCols n = return $ iConst 1

getRowStride n = return 1
getColStride n = return 1

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
genMiniStForInstr (MAdd a b c) = genMAddSt a b c

genMAddSt :: String -> String -> String -> State MiniCodeGenState ()
genMAddSt a b c = do
  maddSt <- maddStTemplate a b c
  addSt maddSt

maddStTemplate :: String -> String -> String -> State MiniCodeGenState (Statement String)
maddStTemplate a b c = iterateOverMatTemplate a (maddBodyTemplate a b c)

maddBodyTemplate :: String -> String -> String -> String -> String -> State MiniCodeGenState (Block String)
maddBodyTemplate a b c rowInd colInd = do
  (aReg, lda) <- loadToRegister a rowInd colInd
  (bReg, ldb) <- loadToRegister b rowInd colInd
  (cReg, ldc) <- loadToRegister c rowInd colInd
  stc <- storeFromRegister c rowInd colInd cReg
  l <- freshLabel
  return $ block [lda, ldb, ldc, plus cReg aReg bReg l, stc]

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
  n <- getNumCols matName
  return (i, \body -> for i (iConst 0) n (iConst 1) body l)

loopOverRows :: String -> State MiniCodeGenState (String, Block String -> Statement String)
loopOverRows matName = do
  l <- freshLabel
  i <- freshIndexVar
  n <- getNumRows matName
  return (i, \body -> for i (iConst 0) n (iConst 1) body l)
  
