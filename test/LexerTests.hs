module LexerTests(allLexerTests) where

import Data.List as L

import Lexer
import TestUtils
import Token

allLexerTests = do
  testFunction (lexString "nofile.lspc") keywordCases
  testFunction (lexString "nofile.lspc") identCases

keywordCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("architecture", dres "architecture"),
   ("output", dres "output"),
   ("operation", dres "operation"),
   ("matrix", dres "matrix"),
   ("C", dres "C"),
   ("rw", dres "rw"),
   ("r", dres "r"),
   ("gen", dres "gen"),
   ("{", dres "{"),
   ("}", dres "}"),
   ("(", dres "("),
   (")", dres ")"),
   (",", dres ",")]

identCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("arc", dident "arc"),
   ("rwoutput", dident "rwoutput"),
   ("casey12", dident "casey12"),
   ("ROGERTHAT", dident "ROGERTHAT")]
  
