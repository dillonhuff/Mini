module Parser(parseOperation) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim

import IndexExpression
import MatrixOperation
import SymbolTable
import Token

parseOperation :: String -> [Token] -> Either String [MatrixOperation]
parseOperation sourceFileName toks = case parse (many pOperation) sourceFileName toks of
  Left err -> Left $ show err
  Right matOp -> Right matOp

pOperation = do
  position <- getPosition
  pResWithNameTok "operation"
  (name, pos) <-pIdent
  pResWithNameTok "("
  args <- pArgList
  pResWithNameTok ")"
  pResWithNameTok "{"
  pResWithNameTok "}"
  return $ matrixOperation name args [] position

pArgList = sepBy pFormalParam (pResWithNameTok ",")

pFormalParam = do
  readMod <- pReadMod
  pResWithNameTok "matrix"
  dataType <- pEntryType
  matLayout <- pLayout
  (name, pos) <- pIdent
  return $ (name, mOpSymInfo arg dataType matLayout)

pReadMod = (pResWithNameTok "output") <|> (pResWithNameTok "r") <|> (pResWithNameTok "rw")

pEntryType = do
  outN <- pResWithNameTok "double" <|> pResWithNameTok "float"
  case resName outN of
    "double" -> return doubleFloat
    "single" -> return singleFloat

pLayout = do
  nr <- pIntLit
  nc <- pIntLit
  rs <- pIntLit
  cs <- pIntLit
  return $ layout (iConst nr) (iConst nc) (iConst rs) (iConst cs)

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

