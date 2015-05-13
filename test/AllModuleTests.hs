module AllModuleTests(main) where

import IndexExpressionTests
import LexerTests
import LoopAnalysisTests
import MatrixOperationTests
import MOpTests
import ParserTests
import RestrictedLayoutTests
import SyntaxTests

main = do
  allIndexExpressionTests
  allLexerTests
  allLoopAnalysisTests
  allMatrixOperationTests
  allMOpTests
  allParserTests
  allRestrictedLayoutTests
  allSyntaxTests
