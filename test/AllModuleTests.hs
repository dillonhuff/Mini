module AllModuleTests(main) where

import DependenceAnalysisTests
import IndexExpressionTests
import LexerTests
import LoopAnalysisTests
import MatrixOperationTests
import MOpTests
import ParserTests
import RestrictedLayoutTests
import SyntaxTests
import TestCaseGenerationTests

main = do
  allDependenceAnalysisTests
  allIndexExpressionTests
  allLexerTests
  allLoopAnalysisTests
  allMatrixOperationTests
  allMOpTests
  allParserTests
  allRestrictedLayoutTests
  allSyntaxTests
  allTestCaseGenerationTests
