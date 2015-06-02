\documentclass{article}

%include lhs2TeX.fmt

\begin{document}

\title{Generate random numbers}
\author{Dillon Huff}
\maketitle

\section{Purpose}

This module provides facilities for generating random values. It
is mostly a convenience wrapper to add some functionality on
top of the MonadRandom library.

\section{Front matter}

\begin{code}

module Utils.GenerateRandomValues(assignRandomValuesInRange) where

import Control.Monad.Random
import Data.List as L
import Data.Map as M

\end{code}

\section{Assigning random values}

This code provides a way to assign random values to elements of a list.

\begin{code}

assignRandomValuesInRange :: (Random a, Ord b, MonadRandom m) => a -> a -> [b] -> m (Map b a)
assignRandomValuesInRange lo hi l = do
  randVals <- getRandomRs (lo, hi)
  return $ M.fromList $ L.zip l (L.take (length l) randVals)

\end{code}

\end{document}
