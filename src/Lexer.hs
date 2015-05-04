module Lexer(lexString) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as Tok

import Token

lexString :: String -> String -> Either String [Token]
lexString sourceFileName str = case parse (sepBy pTok spaces) sourceFileName str of
  Left err -> Left $ show err
  Right toks -> Right toks

languageDef =
  emptyDef { Tok.commentStart    = "/*",
             Tok.commentEnd      = "*/",
             Tok.commentLine     = "//",
             Tok.identStart      = letter,
             Tok.identLetter     = alphaNum,
             Tok.reservedNames   = [ "architecture", "operation", "C", "matrix", "gen", "output", "r", "rw", "double", "float"],
             Tok.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "||", "&&", "~"] }

lexer = Tok.makeTokenParser languageDef

pTok :: Parser Token
pTok = pVarOrRes

pVarOrRes = (try pIdentifier) <|> pResWord <|> pLit

pIdentifier = do
  pos <- getPosition
  first <- pVarName
  return $ ident first pos

pVarName = Tok.identifier lexer

pResWord = do
  pos <- getPosition
  resStr <- try (string "operation")
         <|> string "architecture"
         <|> string "output"
         <|> try (string "rw")
         <|> string "r"
         <|> string "output"
         <|> string "C"
         <|> string "matrix"
         <|> string "gen"
         <|> string "double"
         <|> string "float"
         <|> string "{"
         <|> string "}"
         <|> string "("
         <|> string ")"
         <|> string ","
         <|> string "="
         <|> string ";"
         <|> string "+"
         <|> string "-"
         <|> string "*"
         <|> string "'"
  return $ res resStr pos

pLit = do
  pos <- getPosition
  digs <- many1 digit
  return $ intLit (read digs) pos
