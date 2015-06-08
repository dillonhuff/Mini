module AllModuleTests(main) where

import Analysis.DependenceAnalysisTests
import Analysis.LoopAnalysisTests
import Analysis.RegisterReductionTests
import Core.IndexExpressionTests
import Core.MatrixOperationTests
import Core.MOpTests
import Core.SyntaxTests
import FrontEnd.LexerTests
import FrontEnd.ParserTests
import Testing.RestrictedLayoutTests
import Testing.TestCaseGenerationTests

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
