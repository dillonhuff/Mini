module BackEnd.MiniToC(toCFunc, toCBlock) where

import Data.List as L

import BackEnd.CGen
import Core.IndexExpression
import Core.MiniOperation
import Core.MiniSyntax
import Core.SymbolTable

toCFunc :: a -> Operation a -> CTopLevelItem a
toCFunc dummyAnn op = cFuncDecl cVoid (getOpName op) cArgs cCodeBlock
  where
    cArgs = L.map (\(n, tp) -> (toCType tp, n)) $ getOpArguments op
    cCodeBlock = toCBlock dummyAnn (getMiniOpSymtab op) (getOpLocalVars op) (getOpBlock op)

toCBlock :: a -> MiniSymtab -> [(String, Type)] -> Block a -> CBlock a
toCBlock dummyAnn symT decls blk = cBlock cDecls cStmts
  where
    stmts = blockStatements blk
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
    bufSz = iExprToCExpr $ Core.SymbolTable.getBufferSize name symT

freeBuffers dummyAnn bufNames symT =
  L.map (\n -> freeBuffer dummyAnn n symT) bufNames
  
freeBuffer dummyAnn bufName symT =
  cExprSt (cFuncall "free" [cVar bufName]) dummyAnn
