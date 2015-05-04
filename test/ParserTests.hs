module ParserTests(allParserTests) where

import Data.List as L

import IndexExpression
import Lexer
import MatrixOperation
import Parser
import SymbolTable
import TestUtils
import Token

allParserTests = do
  testFunction (lexAndParseOperation "noname.lspc") opCases

opCases =
  L.map (\(x, y) -> (x, Right [y]))
  [("operation nothing() { }",
    matrixOperation "nothing" [] [] dummyPos),
   ("operation trans(output matrix double 12 32 1 12 b, r matrix double 32 12 1 32 a) {}",
    matrixOperation "trans" [("b", mOpSymInfo arg doubleFloat (layout (iConst 12) (iConst 32) (iConst 1) (iConst 12))),
                             ("a", mOpSymInfo arg doubleFloat (layout (iConst 32) (iConst 12) (iConst 1) (iConst 32)))] [] dummyPos),
   ("operation madd(rw matrix float 12 32 1 12 b, r matrix float 12 32 1 32 a, output matrix float 12 32 32 1 c) { c = a + b; }",
    matrixOperation "madd" [("b", mOpSymInfo arg singleFloat (layout (iConst 12) (iConst 32) (iConst 1) (iConst 12))),
                             ("a", mOpSymInfo arg singleFloat (layout (iConst 12) (iConst 32) (iConst 1) (iConst 32))),
                             ("c", mOpSymInfo arg singleFloat (layout (iConst 12) (iConst 32) (iConst 32) (iConst 1)))]
    [dMatAsg "c" (dMatrixAdd (dMatName "a") (dMatName "b"))] dummyPos)]


lexAndParseOperation fName str = (lexString fName str) >>= (parseOperation fName)


