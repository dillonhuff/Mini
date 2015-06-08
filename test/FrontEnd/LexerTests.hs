module FrontEnd.LexerTests(allLexerTests) where

import Data.List as L

import FrontEnd.Lexer
import TestUtils
import FrontEnd.Token

allLexerTests = do
  testFunction (lexString "nofile.lspc") keywordCases
  testFunction (lexString "nofile.lspc") identCases
  testFunction (lexString "nofile.lspc") litCases

keywordCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("architecture", dres "architecture"),
   ("output", dres "output"),
   ("operation", dres "operation"),
   ("matrix", dres "matrix"),
   ("LangC", dres "LangC"),
   ("rw", dres "rw"),
   ("r", dres "r"),
   ("gen", dres "gen"),
   ("double", dres "double"),
   ("float", dres "float"),
   ("{", dres "{"),
   ("}", dres "}"),
   ("(", dres "("),
   (")", dres ")"),
   (",", dres ","),
   ("=", dres "="),
   (";", dres ";"),
   ("+", dres "+"),
   ("-", dres "-"),
   ("*", dres "*"),
   (".*", dres ".*"),
   ("'", dres "'")]

identCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("arc", dident "arc"),
   ("rwoutput", dident "rwoutput"),
   ("casey12", dident "casey12"),
   ("ROGERTHAT", dident "ROGERTHAT")]
  
litCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("12", dIntLit 12)]
