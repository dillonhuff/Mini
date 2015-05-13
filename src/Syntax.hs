module Syntax(toCType,
              transformBlock,
              MiniSymtab,
              localVars, arguments,
              Statement,
              transformStatementIExprs,
              Type,
              Block,
              toCBlock,
              block,
              load, loadConst, store, plus, minus, times, for,
              forStart, forEnd, forInc, isFor,
              sReg, buffer,
              doubleLit, floatLit, getLitType) where

import Data.List as L
import Data.Map as M

import CGen
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

transformBlock :: (Statement a -> Statement a) -> Block a -> Block a
transformBlock f (Block stmts) = block $ L.map (transformStatement f) stmts

data Statement a
  = BOp Binop String String String a
  | Load String String IExpr a
  | LoadConst String Lit a
  | Store String IExpr String a
  | For String IExpr IExpr IExpr (Block a) a
    deriving (Eq, Ord, Show)

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

isFor (For _ _ _ _ _ _) = True
isFor _ = False

forStart (For _ start _ _ _ _) = start
forInc (For _ _ inc _ _ _) = inc
forEnd (For _ _ _ end _ _) = end

transformStatement :: (Statement a -> Statement a) -> Statement a -> Statement a
transformStatement f (For v s i e blk ann) = f (For v s i e (transformBlock f blk) ann)
transformStatement f s = f s

transformStatementIExprs :: (IExpr -> IExpr) -> Statement a -> Statement a
transformStatementIExprs f (For v s i e blk ann) = For v (f s) (f i) (f e) blk ann
transformStatementIExprs f s = s

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
