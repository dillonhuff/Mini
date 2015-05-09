\documentclass{article}

%include lhs2TeX.fmt

\begin{document}

\title{Matrix Operation}
\author{Dillon Huff}
\maketitle

\section {Purpose}

This module describes the syntax of the language produced by the
Parser module. It is very close to the written syntax of the library
specifications that Mini is intended to process. The three major purposes
of this stage are to

\begin{itemize}

\item check that the operation specification is well formed

\item resolve ambiguities that are allowed in the specification language
syntax e.g. the user does not have to declare or specify layouts for temporary
variables.

\item translate the MatrixOperation into an MOp for further processing and
optimization

\end{itemize}

\section {Module front matter}

\begin{code}
module MatrixOperation(MatrixOperation,
                       typeCheckMatrixOperation,
                       matAsg,
                       dMatAsg,
                       dMatrixOperation,
                       matrixOperation,
                       matrixOperationToMOp,
                       getMatrixOpSymtab,
                       MatrixStmt,
                       matName, matrixAdd, matrixSub, matrixMul, matrixTrans, scalarMul,
                       dMatName, dMatrixAdd, dMatrixSub, dMatrixMul, dMatrixTrans, dScalarMul,
                       simplifySymtab,
                       typeCheckStmt) where

import Control.Lens
import Control.Monad
import Control.Monad.State.Lazy
import Data.List as L
import Text.Parsec.Pos

import MOpCodeGen
import MOpSyntax
import SymbolTable
import Token

\end{code}

\section {Core data structures}

The top level data structure MatrixOperation represents a single operation
in a library specification. Note that as usual data construction functions
that are prefixed with the letter d are constructors for dummy data items
used in testing.

\begin{code}

data MatrixOperation
  = MatrixOperation String MOpSymtab [MatrixStmt] SourcePos
    deriving (Ord, Show)

instance Eq MatrixOperation where
  (==) (MatrixOperation s1 sym1 stmts1 _) (MatrixOperation s2 sym2 stmts2 _) =
    s1 == s2 && sym1 == sym2 && stmts1 == stmts2

matrixOperation name symt stmts sp = MatrixOperation name (mOpSymtab symt) stmts sp

dMatrixOperation name symt stmts = matrixOperation name symt stmts dummyPos

getMatrixOpSymtab (MatrixOperation _ st _ _) = st

data MatrixStmt
  = MStmt String MExpr SourcePos
    deriving (Ord, Show)

instance Eq MatrixStmt where
  (==) (MStmt n1 e1 _) (MStmt n2 e2 _) = n1 == n2 && e1 == e2

matAsg n e p = MStmt n e p
dMatAsg n e = MStmt n e dummyPos

data MExpr
  = MatBinop MatBOp MExpr MExpr SourcePos
  | MatUnop MatUOp MExpr SourcePos
  | VarName String SourcePos
    deriving (Ord, Show)

instance Eq MExpr where
  (==) (MatBinop b1 l1 r1 _) (MatBinop b2 l2 r2 _) = b1 == b2 && l1 == l2 && r1 == r2
  (==) (MatUnop u1 n1 _) (MatUnop u2 n2 _) = u1 == u2 && n1 == n2
  (==) (VarName n1 _) (VarName n2 _) = n1 == n2

dMatrixAdd a b = MatBinop MatAdd a b dummyPos
dMatrixSub a b = MatBinop MatSub a b dummyPos
dMatrixMul a b = MatBinop MatMul a b dummyPos
dScalarMul a b = scalarMul a b dummyPos
dMatrixTrans b = MatUnop MatTrans b dummyPos
dMatName str = VarName str dummyPos

matrixMul a b p = MatBinop MatMul a b p
scalarMul a b p = MatBinop ScalMul a b p
matrixSub a b p = MatBinop MatSub a b p
matrixAdd a b p = MatBinop MatAdd a b p
matrixTrans b p = MatUnop MatTrans b p
matName s p = VarName s p

data MatBOp
  = MatMul
  | MatAdd
  | MatSub
  | ScalMul
    deriving (Eq, Ord, Show)

data MatUOp
  = MatTrans
    deriving (Eq, Ord, Show)

\end{code}

\section {Conversion to MOp}

\begin{code}
matrixOperationToMOp (MatrixOperation name sym stmts _) =
  let initMOp = mOpCodeGen (mOp name sym [])
      instrState = matrixStmtsToMInstrs stmts in
  view mcgMOp $ execState instrState initMOp

addInstr :: MInstr -> State MOpCodeGen ()
addInstr instr = do
  cg <- get
  put $ over (mcgMOp . mOpInstrs) (\is -> is ++ [instr]) cg
  return ()

matrixStmtsToMInstrs :: [MatrixStmt] -> State MOpCodeGen ()
matrixStmtsToMInstrs [] = return ()
matrixStmtsToMInstrs (st:stmts) = do
  matrixStToMInstrs st
  matrixStmtsToMInstrs stmts

matrixStToMInstrs :: MatrixStmt -> State MOpCodeGen ()
matrixStToMInstrs (MStmt n expr _) = do
  (eRes, eInfo) <- matrixExprToMInstrs expr
  addInstr $ masg eRes n

freshTempVar = do
  cg <- get
  let name = "tmp" ++ (show $ view mcgNextInt cg) in
    do
      put $ over mcgNextInt (+1) cg
      return name

addTmpToSymtab :: MOpSymInfo -> State MOpCodeGen String
addTmpToSymtab symInf = do
  cg <- get
  nextName <- freshTempVar
  put $ over (mcgMOp . mOpSymT) (\s -> addMOpEntry nextName symInf s) cg
  return nextName

matrixExprToMInstrs :: MExpr -> State MOpCodeGen (String, MOpSymInfo)
matrixExprToMInstrs (VarName n _) = do
  cg <- get
  return (n, getMOpSymInfo n id (view (mcgMOp . mOpSymT) cg))
matrixExprToMInstrs (MatBinop MatAdd a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  op <- get
  newName <- addTmpToSymtab aInfo
  addInstr $ madd aName bName newName
  return (newName, aInfo)
matrixExprToMInstrs (MatBinop MatSub a b _) = do
  (aName, aInfo) <- matrixExprToMInstrs a
  (bName, bInfo) <- matrixExprToMInstrs b
  op <- get
  newName <- addTmpToSymtab aInfo
  addInstr $ msub aName bName newName
  return (newName, aInfo)

\end{code}

\section{Type checking and temporary variable resolution}

\begin{code}
typeCheckMatrixOperation :: MatrixOperation -> MatrixOperation
typeCheckMatrixOperation (MatrixOperation name symtab stmts p) =
  MatrixOperation name (typeCheckStmts stmts symtab) stmts p

typeCheckStmts :: [MatrixStmt] -> MOpSymtab -> MOpSymtab
typeCheckStmts stmts st =
  L.foldl (\symTab stmt -> typeCheckStmt stmt symTab) st stmts

typeCheckStmt :: MatrixStmt -> MOpSymtab -> MOpSymtab
typeCheckStmt stmt st = execState (tcStmt stmt) st

tcStmt :: MatrixStmt -> State MOpSymtab ()
tcStmt (MStmt n e _) = do
  (l, t) <- simplifyStWithExpr e
  st <- get
  case containsSymbol n st of
    True -> do
      doAssignmentSubstitutions (getMOpSymInfo n getLayout st) l
      return ()
    False -> do
      put $ addMOpEntry n (mOpSymInfo local t l) st
      return ()

simplifySymtab :: MExpr -> MOpSymtab -> MOpSymtab
simplifySymtab e st = execState (simplifyStWithExpr e) st

simplifyStWithExpr :: MExpr -> State MOpSymtab (Layout, EntryType)
simplifyStWithExpr (VarName n _) = do
  st <- get
  return $ (getMOpSymInfo n getLayout st, getEntryType n st)
simplifyStWithExpr (MatUnop u a _) = do
  (aL, t) <- simplifyStWithExpr a
  resL <- simplifyLayoutsUOp u aL
  return (resL, t)
simplifyStWithExpr (MatBinop b l r _) = do
  (leftL, _) <- simplifyStWithExpr l
  (rightL, t) <- simplifyStWithExpr r
  resL <- simplifyLayoutsBOp b leftL rightL
  return (resL, t)

simplifyLayoutsUOp :: MatUOp -> Layout -> State MOpSymtab Layout
simplifyLayoutsUOp MatTrans l = do
  return $ layout (view nc l) (view nr l) (view cs l) (view rs l)

simplifyLayoutsBOp :: MatBOp -> Layout -> Layout -> State MOpSymtab Layout
simplifyLayoutsBOp MatMul leftL rightL = do
  l1 <- doSubstitution (view nc leftL) (view nr rightL) leftL
  return $ layout (view nr l1) (view nc rightL) (view rs l1) (view cs l1)
simplifyLayoutsBOp ScalMul _ rightL = do
  return rightL
simplifyLayoutsBOp b leftL rightL = do
  l1 <- doSubstitution (view nr leftL) (view nr rightL) leftL
  l2 <- doSubstitution (view nc leftL) (view nc rightL) l1
  return l2

doAssignmentSubstitutions :: Layout -> Layout -> State MOpSymtab Layout
doAssignmentSubstitutions targetL resultL = do
  newL <- doSubstitution (view nr targetL) (view nr resultL) targetL
  resL <- doSubstitution (view nc newL) (view nc resultL) newL
  return resL

doSubstitution l r oldLayout = do
  st <- get
  put $ subInStLayouts l r st
  return $ subInLayout l r oldLayout
  
\end{code}

\end{document}
