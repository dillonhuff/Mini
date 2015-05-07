module IndexExpression(IExpr,
                       evaluateIExprConstants,
                       iAdd, iMul, iConst, iVar, iSub,
                       iExprToCExpr,
                       subIExpr) where

import CGen

data IExpr
  = IConst Int
  | IVar String
  | IMul IExpr IExpr
  | IAdd IExpr IExpr
    deriving (Eq, Ord)

iSub l r = iAdd l (iMul (iConst (-1)) r)
iAdd l r = IAdd l r
iMul l r = IMul l r
iConst i = IConst i
iVar n = IVar n

subIExpr target result i =
  case i == target of
    True -> result
    False -> subInSubIExprs target result i

subInSubIExprs target result (IMul l r) =
  IMul (subIExpr target result l) (subIExpr target result r)
subInSubIExprs target result (IAdd l r) =
  IAdd (subIExpr target result l) (subIExpr target result r)
subInSubIExprs _ _ i = i

isConstant (IConst n) = True
isConstant _ = False

constValue (IConst n) = n
constValue other = error $ show other ++ " is not a constant"

iExprToCExpr (IVar n) = cVar n
iExprToCExpr (IConst i) = cIntLit i
iExprToCExpr (IAdd l r) = cAdd (iExprToCExpr l) (iExprToCExpr r)
iExprToCExpr (IMul l r) = cMul (iExprToCExpr l) (iExprToCExpr r)

instance Show IExpr where
  show (IVar n) = n
  show (IConst it) = show it
  show (IAdd l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (IMul l r) = "(" ++ show l ++ " * " ++ show r ++ ")"

evaluateIExprConstants :: IExpr -> IExpr
evaluateIExprConstants (IMul l r) =
  case isConstant evaledL && isConstant evaledR of
    True -> iConst $ (constValue evaledL) * (constValue evaledR)
    False -> IMul evaledL evaledR
  where
    evaledL = evaluateIExprConstants l
    evaledR = evaluateIExprConstants r
evaluateIExprConstants (IAdd l r) =
  case isConstant evaledL && isConstant evaledR of
    True -> iConst $ (constValue evaledL) + (constValue evaledR)
    False -> IAdd evaledL evaledR
  where
    evaledL = evaluateIExprConstants l
    evaledR = evaluateIExprConstants r
evaluateIExprConstants e = e  
