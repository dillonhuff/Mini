module AllModuleTests(main) where

import IndexExpressionTests
import LexerTests
import MatrixOperationTests
import MOpTests
import ParserTests
import SyntaxTests

main = do
  allIndexExpressionTests
  allLexerTests
  allMatrixOperationTests
  allMOpTests
  allParserTests
  allSyntaxTests
