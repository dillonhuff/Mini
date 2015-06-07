module FrontEnd.Parser(parseOperation,
              parseStatement,
              parseFormalParam) where

import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

import Core.IndexExpression
import Core.MatrixOperation
import Core.SymbolTable
import FrontEnd.Token

parser :: Parsec [Token] () a -> String -> [Token] -> Either String a
parser p srcName toks = case parse p srcName toks of
  Left err -> Left $ show err
  Right res -> Right $ res

parseOperation :: String -> [Token] -> Either String [MatrixOperation]
parseOperation sourceFileName toks = case parse (many pOperation) sourceFileName toks of
  Left err -> Left $ show err
  Right matOp -> Right matOp

parseStatement :: String -> [Token] -> Either String MatrixStmt
parseStatement sourceFileName toks = case parse pMatStatement sourceFileName toks of
  Left err -> Left $ show err
  Right matOp -> Right matOp

parseFormalParam srcName toks = parser pFormalParam srcName toks

pOperation = do
  position <- getPosition
  pResWithNameTok "operation"
  (name, pos) <-pIdent
  pResWithNameTok "("
  args <- pArgList
  pResWithNameTok ")"
  pResWithNameTok "{"
  ops <- pMatStatements
  pResWithNameTok "}"
  return $ matrixOperation name args ops position

pArgList = sepBy pFormalParam (pResWithNameTok ",")

pFormalParam = do
  readMod <- pReadMod
  pResWithNameTok "matrix"
  dataType <- pEntryType
  matLayout <- pLayout
  (name, pos) <- pIdent
  return $ (name, mOpSymInfo arg dataType (matLayout name))

pReadMod = (pResWithNameTok "output") <|> (pResWithNameTok "r") <|> (pResWithNameTok "rw")

pEntryType = do
  outN <- pResWithNameTok "double" <|> pResWithNameTok "float"
  case resName outN of
    "double" -> return doubleFloat
    "float" -> return singleFloat

pLayout = do
  nr <- pDim
  nc <- pDim
  rs <- pDim
  cs <- pDim
  return $ layoutForMat nr nc rs cs

layoutForMat nr nc rs cs name =
  layout (lr nr "_nrows") (lr nc "_ncols") (lr rs "_rs") (lr cs "_cs")
  where
    lr i suffix = if i == iVar "gen" then (iVar (name ++ suffix)) else i

pDim = pGen <|> pConstDim

pGen = do
  pResWithNameTok "gen"
  return $ iVar "gen"

pConstDim = do
  val <- pIntLit
  return $ iConst val

pMatStatements = many pMatStatement

pMatStatement = do
  pos <- getPosition
  (name, pos) <- pIdent
  pResWithNameTok "="
  ex <- pMatExpr
  pResWithNameTok ";"
  return $ matAsg name ex pos

table =
  [[matTransOp],
   [matMulOp, scalMulOp],
   [matAddOp, matSubOp]]

matTransOp = Postfix pMatTransOp
matMulOp = Infix pMatMulOp AssocLeft
scalMulOp = Infix pScalMulOp AssocLeft
matAddOp = Infix pMatAddOp AssocLeft
matSubOp = Infix pMatSubOp AssocLeft

pMatTransOp = do
  pos <- getPosition
  pResWithNameTok "'"
  return $ (\n -> matrixTrans n pos)

pScalMulOp = do
  pos <- getPosition
  pResWithNameTok ".*"
  return $ (\l r -> scalarMul l r pos)

pMatMulOp = do
  pos <- getPosition
  pResWithNameTok "*"
  return $ (\l r -> matrixMul l r pos)
  
pMatAddOp = do
  pos <- getPosition
  pResWithNameTok "+"
  return $ (\l r -> matrixAdd l r pos)

pMatSubOp = do
  pos <- getPosition
  pResWithNameTok "-"
  return $ (\l r -> matrixSub l r pos)

pTerm = pMatName
      <|> pParens pMatExpr

pMatExpr = buildExpressionParser table pTerm

pMatName = do
  (n, pos) <- pIdent
  return $ matName n pos

pParens pOther = do
  pResWithNameTok "("
  ot <- pOther
  pResWithNameTok ")"
  return ot

pIntLit :: Monad m => ParsecT [Token] u m Int
pIntLit = do
  intL <- pIntLitTok
  return $ litInt intL

pIdent :: Monad m => ParsecT [Token] u m (String, SourcePos)
pIdent = do
  position <- getPosition
  idT <- pIdentTok
  return $ (identName idT, position)

pIntLitTok :: Monad m => ParsecT [Token] u m Token
pIntLitTok = mTok (\t -> isIntLit t)

pIdentTok :: Monad m => ParsecT [Token] u m Token
pIdentTok = mTok (\t -> isIdent t)

pResWithNameTok :: Monad m => String -> ParsecT [Token] u m Token
pResWithNameTok name = mTok (\t -> isRes t && resName t == name)

mTok :: (Monad m) => (Token -> Bool) -> ParsecT [Token] u m Token
mTok condition = tokenPrim show updatePos meetsCond
  where
    meetsCond t = if condition t then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position

