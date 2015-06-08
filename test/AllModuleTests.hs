module AllModuleTests(main) where

import Analysis.DependenceAnalysisTests
import Analysis.LoopAnalysisTests
import Analysis.RegisterReductionTests
import IndexExpressionTests
import LexerTests
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
  allRegisterReductionTests
  allRestrictedLayoutTests
  allSyntaxTests
  allTestCaseGenerationTests
