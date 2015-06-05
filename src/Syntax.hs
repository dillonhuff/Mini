module Syntax(toCType,
              transformBlock,
              MiniSymtab,
              localVars, arguments,
              Statement,
              transformStatementIExprs,
              label, nonLoopStatements, substituteName,
              Operand, operandWritten, operandsRead,
              operandsHaveSameType, isBufferVal,
              bufferName, registerName, 
              Type,
              Block,
              noLoopsInBlock, updateBlock, subIExprInBlock,
              blockStatements, expandBlockStatements,
              updateBlockM,
              toCBlock,
              block,
              load, loadConst, store, plus, minus, times, for,
              forStart, forEnd, forInc, isFor, forInductionVariable, forBody,
              isLoad, isStore,
              sReg, buffer, accessIExpr,
              doubleLit, floatLit, getLitType) where

import Control.Monad
import Data.List as L
import Data.Map as M

import BackEnd.CGen
import IndexExpression
import SymbolTable

prototype :: String -> MiniSymtab -> String
prototype n st = "void " ++ n ++ "(" ++ argumentStr st ++ ")"

argumentStr :: MiniSymtab -> String
argumentStr st = L.concat $ L.intersperse ", " $ L.map (\(n, tp) -> show tp ++ " " ++ n) $ arguments st

toCBlock :: a -> MiniSymtab -> [(String, Type)] -> Block a -> CBlock a
toCBlock dummyAnn symT decls (Block stmts) = cBlock cDecls cStmts
  where
    cDecls = L.map (\(n, tp) -> (toCType tp, n)) decls
    tmpBuffers = getTmpBuffers symT
    allocStmts = allocateBuffers dummyAnn tmpBuffers symT
    bodyStmts = L.map toCStmt $ stmts
    deallocStmts = freeBuffers dummyAnn tmpBuffers symT
    cStmts = allocStmts ++ bodyStmts ++ deallocStmts

allocateBuffers dummyAnn bufNames symT =
  L.map (\n -> allocBuf dummyAnn n symT) bufNames

allocBuf dummyAnn name symT =
  cExprSt (cAssign (cVar name) (cFuncall "malloc" [cMul (cSizeOf bufTp) bufSz])) dummyAnn
  where
    bufTp = toCType $ getBufferType name symT
    bufSz = iExprToCExpr $ getBufferSize name symT

freeBuffers dummyAnn bufNames symT =
  L.map (\n -> freeBuffer dummyAnn n symT) bufNames
  
freeBuffer dummyAnn bufName symT =
  cExprSt (cFuncall "free" [cVar bufName]) dummyAnn

data Block a
  = Block [Statement a]
    deriving (Eq, Ord, Show)

block = Block

blockStatements (Block stmts) = stmts

transformBlock :: (Statement a -> Statement a) -> Block a -> Block a
transformBlock f (Block stmts) = block $ L.map (transformStatement f) stmts

transformBlockM :: (Monad m) => (Statement a -> m (Statement a)) -> Block a -> m (Block a)
transformBlockM f (Block stmts) = do
  newStmts <- sequence $ L.map (transformStatementM f) stmts
  return $ block newStmts

updateBlock :: (Block a -> Block a) -> Block a -> Block a
updateBlock f b = f $ block $ L.map (transformStatement (updateStmtBlocks f)) $ blockStatements b

updateBlockM :: (Monad m) => (Block a -> m (Block a)) -> Block a -> m (Block a)
updateBlockM f b = do
  modStmts <- sequence $ L.map (transformStatementM (updateStmtBlocksM f)) $ blockStatements b
  f (block modStmts)

expandBlockStatements :: (Statement a -> [Statement a]) -> Block a -> Block a
expandBlockStatements f (Block stmts) = block $ L.concatMap (expandStatement f) stmts

noLoopsInBlock b = L.and $ L.map (\st -> not $ isFor st) $ blockStatements b

data Statement a
  = BOp Binop String String String a
  | Load String String IExpr a
  | LoadConst String Lit a
  | Store String IExpr String a
  | For String IExpr IExpr IExpr (Block a) a
    deriving (Eq, Ord, Show)

label (BOp _ _ _ _ l) = l
label (Load _ _ _ l) = l
label (LoadConst _ _ l) = l
label (Store _ _ _ l) = l
label (For _ _ _ _ _ l) = l

nonLoopStatements (For _ _ _ _ b _) = L.concatMap nonLoopStatements $ blockStatements b
nonLoopStatements st = [st]

toCStmt (For n start inc end (Block bodyStatements) ann) =
  cFor (cAssign (cVar n) (iExprToCExpr start))
       (cLEQ (cVar n) (iExprToCExpr end))
       (cAssign (cVar n) (cAdd (cVar n) (iExprToCExpr inc)))
       (cBlock [] $ L.map toCStmt bodyStatements) ann
toCStmt (LoadConst n lit ann) = cExprSt (cAssign (cVar n) (miniLitToCLit lit)) ann
toCStmt (Load n1 n2 iExpr ann) = cExprSt (cAssign (cVar n1) (cArrAcc (cVar n2) (iExprToCExpr iExpr))) ann
toCStmt (BOp Plus s1 s2 s3 ann) = cExprSt (cAssign (cVar s1) (cAdd (cVar s2) (cVar s3))) ann
toCStmt (BOp Minus s1 s2 s3 ann) = cExprSt (cAssign (cVar s1) (cSub (cVar s2) (cVar s3))) ann
toCStmt (BOp Times s1 s2 s3 ann) = cExprSt (cAssign (cVar s1) (cMul (cVar s2) (cVar s3))) ann
toCStmt (Store s1 iExpr s2 ann) = cExprSt (cAssign (cArrAcc (cVar s1) (iExprToCExpr iExpr)) (cVar s2)) ann

loadConst n l = LoadConst n l
load = Load
store = Store
plus = BOp Plus
minus = BOp Minus
times = BOp Times
for n start inc end b ann = For n start inc end b ann

operandsRead (BOp _ _ a b _) = [reg a, reg b]
operandsRead (Store _ _ a _) = [reg a]
operandsRead (Load _ b i _) = [bufferVal b i]
operandsRead (LoadConst _ _ _) = []

operandWritten (BOp _ c _ _ _) = reg c
operandWritten (Store a i _ _) = bufferVal a i
operandWritten (LoadConst a _ _) = reg a
operandWritten (Load a _ _ _) = reg a

isLoad (Load _ _ _ _) = True
isLoad _ = False

isStore (Store _ _ _ _) = True
isStore _ = False

isFor (For _ _ _ _ _ _) = True
isFor _ = False

forStart (For _ start _ _ _ _) = start
forInc (For _ _ inc _ _ _) = inc
forEnd (For _ _ _ end _ _) = end
forInductionVariable (For i _ _ _ _ _) = i
forBody (For _ _ _ _ b _) = b

transformStatement :: (Statement a -> Statement a) -> Statement a -> Statement a
transformStatement f (For v s i e blk ann) = f (For v s i e (transformBlock f blk) ann)
transformStatement f s = f s

transformStatementM :: (Monad m) => (Statement a -> m (Statement a)) -> Statement a -> m (Statement a)
transformStatementM f (For v s i e blk ann) = do
  newBody <- transformBlockM f blk
  f (For v s i e newBody ann)
transformStatementM f s = f s

updateStmtBlocks f (For v s i e blk ann) = For v s i e (updateBlock f blk) ann
updateStmtBlocks f other = other

updateStmtBlocksM f (For v s i e blk ann) = do
  newBody <- updateBlockM f blk
  return $ For v s i e newBody ann
updateStmtBlocksM f other = return other

expandStatement :: (Statement a -> [Statement a]) -> Statement a -> [Statement a]
expandStatement f (For v s i e blk ann) = f (For v s i e (expandBlockStatements f blk) ann)
expandStatement f s = f s

transformStatementIExprs :: (IExpr -> IExpr) -> Statement a -> Statement a
transformStatementIExprs f (For v s i e blk ann) = For v (f s) (f i) (f e) blk ann
transformStatementIExprs f (Load n m i ann) = Load n m (f i) ann
transformStatementIExprs f (Store n i m ann) = Store n (f i) m ann
transformStatementIExprs f s = s

substituteName target result (Load n m i ann) = Load (subN target result n) (subN target result m) i ann
substituteName target result (Store n i m ann) = Store (subN target result n) i (subN target result m) ann
substituteName target result (BOp op a b c ann) = BOp op (subN target result a) (subN target result b) (subN target result c) ann

subN target result n =
  case n == target of
    True -> result
    False -> n

data Binop
  = Plus
  | Minus
  | Times
    deriving (Eq, Ord)

instance Show Binop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"

data Lit
  = DoubleLit Double
  | FloatLit Float
    deriving (Eq, Ord, Show)

doubleLit d = DoubleLit d
floatLit f = FloatLit f

getLitType (DoubleLit _) = double
getLitType (FloatLit _) = single

miniLitToCLit (DoubleLit d) = cDoubleLit d
miniLitToCLit (FloatLit f) = cFloatLit f

data Operand
  = Register String
  | BufferVal String IExpr
    deriving (Eq, Ord, Show)

reg s = Register s
bufferVal s i = BufferVal s i

isBufferVal (BufferVal _ _) = True
isBufferVal _ = False

operandsHaveSameType (Register _) (Register _) = True

bufferName (BufferVal s _) = s

accessIExpr (BufferVal _ i) = i

registerName (Register s) = s
registerName other = error $ "cannot get register name for buffer " ++ show other

subIExprInBlock :: IExpr -> String -> Block a -> Block a
subIExprInBlock ie n b = transformBlock (transformStatementIExprs (subIExprForVar ie n)) b
