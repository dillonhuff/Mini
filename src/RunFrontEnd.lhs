\documentclass{article}

%include lhs2TeX.fmt

\begin{document}

\title{Run Front End}
\author{Dillon Huff}
\maketitle

\section{Purpose}

This is the central module for the front end of the Mini compiler.
It brings together code to lex, parse, type check, and generate test
cases for a library specification.

\section{Front matter}

\begin{code}

module RunFrontEnd(runFrontEnd) where

import Control.Monad.Random
import Data.List as L
import Data.Map as M

import Lexer
import Parser
import MatrixOperation
import TestCaseGeneration

\end{code}

\section{Front end runner code}

The runFrontEnd function tries to
process a string read from a library spec file and either fails or produces
a list of matrix operations along with some test cases for each operation.
Because test case generation involves random numbers the matrices and their
test cases are contained in a MonadRandom instance.

\begin{code}

runFrontEnd :: (MonadRandom m) => String -> String -> Either String (m [(MatrixOperation, [Map String Int])])
runFrontEnd fileName libStr = do
  ops <- readLibSpec fileName libStr
  let opsWithTestCases = sequence $ L.map (typeCheckAndGenerateTestCases lowDim highDim) ops in
    return opsWithTestCases

readLibSpec fileName libSpecStr =
  (lexString fileName libSpecStr) >>= (parseOperation fileName)

typeCheckAndGenerateTestCases lo hi matOp =
  let checkedMOp = typeCheckMatrixOperation matOp in
  do
    testCases <- genRowAndColMajorExamples lo hi (getMatrixOpSymtab checkedMOp)
    return (checkedMOp, testCases)

\end{code}

The lowDim and highDim parameters control the range of values assigned in randomized test cases

\begin{code}

lowDim = 2
highDim = 100

\end{code}

\end{document}
