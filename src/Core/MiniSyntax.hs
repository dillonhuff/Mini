module Core.MiniSyntax(toCType,
                       toCStmt,
                       transformBlock,
                       transformStatementList,
                       Statement,
                       transformStatementIExprs,
                       transformStatement,
                       label, setLabel, nonLoopStatements, substituteName,
                       multiSubstitution,
                       operandWritten, operandsRead, allOperands,
                       Type,
                       Block,
                       noLoopsInBlock, updateBlock, subIExprInBlock,
                       expandStatement,
                       blockStatements, expandBlockStatements, expandBlockStatementsM,
                       updateBlockM,
                       block,
                       load, loadConst, store, plus, minus, times, for, regAssign,
                       forStart, forEnd, forInc, isFor, forInductionVariable, forBody,
                       isLoad, isStore, isLoadConst, isRegAssign, isBinop, namesReferenced,
                       sReg, buffer,
                       doubleLit, floatLit, getLitType) where

import Control.Monad
import Data.List as L
import Data.Map as M
import Data.Maybe as Maybe

import BackEnd.CGen
import Core.IndexExpression
import Core.Operand
import Core.SymbolTable

data Block a
  = Block [Statement a]
    deriving (Eq, Ord)

instance Show a => Show (Block a) where
  show (Block stmts) = "{ " ++ L.concat (L.intersperse "; " $ L.map show stmts) ++ " }"

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

expandBlockStatementsM :: (Monad m) => (Statement a -> m [Statement a]) -> Block a -> m (Block a)
expandBlockStatementsM f (Block stmts) = do
  resStmts <- liftM L.concat $ sequence $ L.map (expandStatementM f) stmts
  return $ block resStmts

noLoopsInBlock b = L.and $ L.map (\st -> not $ isFor st) $ blockStatements b

data Statement a
  = BOp Binop String String String a
  | Load String String IExpr a
  | LoadConst String Lit a
  | Store String IExpr String a
  | For String IExpr IExpr IExpr (Block a) a
  | RegAssign String String a
    deriving (Eq, Ord)

instance Show a => Show (Statement a) where
  show (BOp op a b c ann) = a ++ " = " ++ b ++ " " ++ show op ++ " " ++ c ++ " " ++ show ann
  show (Load a b i ann) = a ++ " = load " ++ b ++ " " ++ show i ++ " " ++ show ann
  show (LoadConst a l ann) = a ++ " = " ++ show l ++ " " ++ show ann
  show (Store a i b ann) = "store " ++ a ++ " " ++ show i ++ " " ++ b ++ " " ++ show ann
  show (For v s i e b ann) = "for " ++ v ++ " " ++ show s ++ ":" ++ show i ++ ":" ++ show e ++ " " ++ show b ++ " " ++ show ann
  show (RegAssign a b ann) = a ++ " = " ++ b ++ " " ++ show ann

label (BOp _ _ _ _ l) = l
label (Load _ _ _ l) = l
label (LoadConst _ _ l) = l
label (Store _ _ _ l) = l
label (For _ _ _ _ _ l) = l
label (RegAssign _ _ l) = l

setLabel l (BOp op a b c _) = BOp op a b c l
setLabel l (Load a b e _) = Load a b e l
setLabel l (LoadConst a b _) = LoadConst a b l
setLabel l (Store a e b _) = Store a e b l
setLabel l (For v s i e b _) = For v s i e b l
setLabel l (RegAssign a b _) = RegAssign a b l

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
toCStmt (RegAssign l r ann) = cExprSt (cAssign (cVar l) (cVar r)) ann

loadConst n l = LoadConst n l
load = Load
store = Store
plus = BOp Plus
minus = BOp Minus
times = BOp Times
for n start inc end b ann = For n start inc end b ann
regAssign = RegAssign

operandsRead (BOp _ _ a b _) = [reg a, reg b]
operandsRead (Store _ _ a _) = [reg a]
operandsRead (Load _ b i _) = [bufferVal b i]
operandsRead (LoadConst _ _ _) = []
operandsRead (RegAssign _ b _) = [reg b]

operandWritten (BOp _ c _ _ _) = reg c
operandWritten (Store a i _ _) = bufferVal a i
operandWritten (LoadConst a _ _) = reg a
operandWritten (Load a _ _ _) = reg a
operandWritten (RegAssign a _ _) = reg a

allOperands stmt = (operandWritten stmt) : (operandsRead stmt)

isRegAssign (RegAssign _ _ _) = True
isRegAssign _ = False

isBinop (BOp _ _ _ _ _) = True
isBinop _ = False

isLoadConst (LoadConst _ _ _) = True
isLoadConst _ = False

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

transformStatementList :: ([Statement a] -> [Statement a]) -> [Statement a] -> [Statement a]
transformStatementList f stmts =
  let res = f stmts in
  L.map (transformFors f) res

transformFors f (For v s i e blk ann) = For v s i e (block $ transformStatementList f $ blockStatements blk) ann
transformFors f other = other

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

expandStatementM :: (Monad m) => (Statement a -> m [Statement a]) -> Statement a -> m [Statement a]
expandStatementM f (For v s i e blk ann) = do
  resBody <- expandBlockStatementsM f blk
  f (For v s i e resBody ann)
expandStatementM f s = f s

transformStatementIExprs :: (IExpr -> IExpr) -> Statement a -> Statement a
transformStatementIExprs f (For v s i e blk ann) = For v (f s) (f i) (f e) blk ann
transformStatementIExprs f (Load n m i ann) = Load n m (f i) ann
transformStatementIExprs f (Store n i m ann) = Store n (f i) m ann
transformStatementIExprs f s = s

substituteName target result (Load n m i ann) = Load (subN target result n) (subN target result m) i ann
substituteName target result (Store n i m ann) = Store (subN target result n) i (subN target result m) ann
substituteName target result (BOp op a b c ann) = BOp op (subN target result a) (subN target result b) (subN target result c) ann
substituteName target result (RegAssign a b ann) = RegAssign (subN target result a) (subN target result b) ann
substituteName target result (LoadConst a c ann) = LoadConst (subN target result a) c ann
substituteName _ _ other = other

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

namesReferenced stmt =
  case isFor stmt of
    True -> L.concatMap namesReferenced $ blockStatements $ forBody stmt
    False -> L.map operandName $ allOperands stmt

multiSubstitution [] st = st
multiSubstitution ((targetName, resultName):rest) st =
  multiSubstitution rest $ substituteName targetName resultName st

subIExprInBlock :: IExpr -> String -> Block a -> Block a
subIExprInBlock ie n b = transformBlock (transformStatementIExprs (subIExprForVar ie n)) b
