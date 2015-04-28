module CGen(CTopLevelItem,
            cFuncDecl,
            cInt, cFloat, cDouble, cFILE, cVoid, cPtr,
            CBlock,
            cBlock,
            cAssign, cAdd, cSub, cMul,
            cIntLit, cFloatLit, cDoubleLit,
            cVar, cArrAcc, ) where

import Data.List as L

data CTopLevelItem a
  = CInclude String
  | CFuncDecl (CFunc a)
    deriving (Eq, Ord, Show)

cFuncDecl retType name fParams body = CFuncDecl $ CFunc retType name fParams body

instance Pretty (CTopLevelItem a) where
  prettyPrint indL (CInclude name) = indent indL $ "#include " ++ name
  prettyPrint indL (CFuncDecl cFunc) = prettyPrint indL cFunc

data CFunc a
  = CFunc CType String [(CType, String)] (CBlock a)
    deriving (Eq, Ord, Show)

instance Pretty (CFunc a) where
  prettyPrint indL (CFunc tp name formalParams blk) = show tp ++ " " ++ name ++ "(" ++ fParamsStr ++ ")" ++ prettyPrint indL blk
    where
      fParamsStr = L.concat $ L.intersperse ", " $ L.map (\(tp, n) -> show tp ++ " " ++ n) formalParams

data CType
  = CInt
  | CFloat
  | CDouble
  | CFILE
  | CPtr CType
  | CVoid
    deriving (Eq, Ord, Show)

cInt = CInt
cFloat = CFloat
cDouble = CDouble
cFILE = CFILE
cVoid = CVoid
cPtr t = CPtr t

data CBlock a
  = CBlock [(CType, String)] [CStmt a]
    deriving (Eq, Ord, Show)

cBlock = CBlock

instance Pretty (CBlock a) where
  prettyPrint indL (CBlock decls stmts) =
    indent indL "{\n" ++
    (L.concatMap (\(tp, n) -> indent (indL + 1) $ show tp ++ " " ++ n ++ ";\n") decls) ++
    (L.concatMap (\st -> prettyPrint (indL + 1) st) stmts) ++
    indent indL "\n}\n"

data CStmt a
  = CFor CExpr CExpr CExpr (CBlock a) a
  | CIfThenElse CExpr (CBlock a) (CBlock a) a
  | CBlockStmt (CBlock a) a
  | CFuncall [CExpr] a
  | CAssign CExpr CExpr a
    deriving (Eq, Ord, Show)

instance Pretty (CStmt a) where
  prettyPrint indL (CFor st end inc blk ann) =
    indent indL $ "for () {}"

cAssign = CAssign

data CExpr
  = CIntLit Int
  | CFloatLit Float
  | CDoubleLit Double
  | CVar String
  | CBinop CBinop CExpr CExpr
    deriving (Eq, Ord, Show)

cIntLit = CIntLit
cFloatLit = CFloatLit
cDoubleLit = CDoubleLit
cVar n = CVar n
cArrAcc v e = CBinop ArrAcc v e
cAdd e1 e2 = CBinop Plus e1 e2
cSub e1 e2 = CBinop Minus e1 e2
cMul e1 e2 = CBinop Times e1 e2

data CBinop
  = Plus
  | Minus
  | Times
  | Or
  | ArrAcc
    deriving (Eq, Ord, Show)

class Pretty a where
  prettyPrint :: Int -> a -> String

indent :: Int -> String -> String
indent indL str = (L.replicate indL '\t') ++ str
