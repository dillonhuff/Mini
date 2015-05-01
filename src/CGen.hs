module CGen(CTopLevelItem,
            cFuncDecl, cInclude,
            cInt, cFloat, cDouble, cFILE, cVoid, cPtr,
            CBlock,
            cBlock,
            CStmt,
            CExpr,
            cExprSt, cBlockSt,
            CType,
            cAssign, cAdd, cSub, cMul, cFuncall, cOr, cLEQ, cIfThenElse, cFor,
            cIntLit, cFloatLit, cDoubleLit,
            cVar, cArrAcc, cReturn, cSizeOf,
            getReferencedType,
            Pretty(..)) where

import Data.List as L

data CTopLevelItem a
  = CInclude String
  | CFuncDecl (CFunc a)
    deriving (Eq, Ord, Show)

cInclude str = CInclude str
cFuncDecl retType name fParams body = CFuncDecl $ CFunc retType name fParams body

instance Show a => Pretty (CTopLevelItem a) where
  prettyPrint indL (CInclude name) = indent indL $ "#include " ++ name
  prettyPrint indL (CFuncDecl cFunc) = prettyPrint indL cFunc

data CFunc a
  = CFunc CType String [(CType, String)] (CBlock a)
    deriving (Eq, Ord, Show)

instance Show a => Pretty (CFunc a) where
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
    deriving (Eq, Ord)

instance Show CType where
  show CInt = "int"
  show CFloat = "float"
  show CDouble = "double"
  show CFILE = "FILE"
  show (CPtr t) = show t ++ "*"
  show CVoid = "void"

getReferencedType (CPtr tp) = tp

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

instance Show a => Pretty (CBlock a) where
  prettyPrint indL (CBlock decls stmts) =
    indent indL "{\n" ++
    (L.concatMap (\(tp, n) -> indent (indL + 1) $ show tp ++ " " ++ n ++ ";\n") decls) ++
    (L.concatMap (\st -> prettyPrint (indL + 1) st) stmts) ++
    indent indL "}\n"

data CStmt a
  = CFor CExpr CExpr CExpr (CBlock a) a
  | CIfThenElse CExpr (CBlock a) (CBlock a) a
  | CBlockStmt (CBlock a) a
  | CExprSt CExpr a
  | CReturn CExpr a
  | CBlockSt (CBlock a) a
    deriving (Eq, Ord, Show)

cFor init end inc body a = CFor init end inc body a
cIfThenElse e l r a = CIfThenElse e l r a
cReturn e a = CReturn e a
cExprSt e a = CExprSt e a
cBlockSt d st a = CBlockSt (CBlock d st) a

instance Show a => Pretty (CStmt a) where
  prettyPrint indL (CFor st end inc blk ann) =
    indent indL $ "for (" ++ show st ++ "; " ++ show end ++ "; " ++ show inc ++ ")\n" ++ prettyPrint indL blk
  prettyPrint indL (CReturn e ann) =
    indent indL $ "return " ++ show e ++ ";\n"
  prettyPrint indL (CExprSt e ann) =
    indent indL $ show e ++ ";\n"
  prettyPrint indL (CBlockSt b ann) =
    prettyPrint indL b
  prettyPrint indL (CIfThenElse e l r ann) =
    indent indL $ "if (" ++ show e ++ ")\n" ++ prettyPrint indL l ++ (indent indL "else\n") ++ prettyPrint indL r



data CExpr
  = CIntLit Int
  | CFloatLit Float
  | CDoubleLit Double
  | CVar String
  | CBinop CBinop CExpr CExpr
  | CFuncall String [CExpr]
  | CAssign CExpr CExpr
  | CSizeOf CType
    deriving (Eq, Ord)

instance Show CExpr where
  show = showExpr
  
showExpr (CVar n) = n
showExpr (CBinop ArrAcc l r) = show l ++ "[" ++ show r ++ "]"
showExpr (CBinop op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
showExpr (CIntLit i) = show i
showExpr (CDoubleLit d) = show d
showExpr (CFloatLit f) = show f
showExpr (CSizeOf tp) = "sizeof(" ++ show tp ++ ")"
showExpr (CAssign l r) = show l ++ " = " ++ show r
showExpr (CFuncall n args) =
  n ++ "(" ++ (L.concat $ L.intersperse ", " $ L.map show args) ++ ")"

cAssign = CAssign
cIntLit = CIntLit
cFloatLit = CFloatLit
cDoubleLit = CDoubleLit
cVar n = CVar n
cArrAcc v e = CBinop ArrAcc v e
cAdd e1 e2 = CBinop Plus e1 e2
cSub e1 e2 = CBinop Minus e1 e2
cMul e1 e2 = CBinop Times e1 e2
cOr e1 e2 = CBinop Or e1 e2
cLEQ e1 e2 = CBinop LEQ e1 e2
cSizeOf tp = CSizeOf tp
cFuncall n args = CFuncall n args

data CBinop
  = Plus
  | Minus
  | Times
  | Or
  | LEQ
  | ArrAcc
    deriving (Eq, Ord)

instance Show CBinop where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Or = "||"
  show LEQ = "<="

class Pretty a where
  prettyPrint :: Int -> a -> String

indent :: Int -> String -> String
indent indL str = (L.replicate indL '\t') ++ str
