\documentclass{article}

%include lhs2TeX.fmt

\begin{document}

\title{Map Utils}
\author{Dillon Huff}
\maketitle

\section{Purpose}

This module provides some utility functions for working with Data.Map
that are not provided by the Data.Map library itself.

\section{Front matter}

\begin{code}

module Utils.MapUtils(chain) where

import Data.List as L
import Data.Map as M
import Data.Maybe

\end{code}

\section{Chaining maps together}

\begin{code}

chain :: (Ord a, Ord b) => Map a b -> Map b c -> Map a c
chain aToB bToC =
  let aToBPairs = M.toList aToB
      maybeAToCPairs = L.map (\(a, b) -> maybeNewPair a b bToC) aToBPairs in
  M.fromList $ catMaybes maybeAToCPairs

maybeNewPair :: (Ord b) => a -> b -> Map b c -> Maybe (a, c)
maybeNewPair a b bToC = do
  c <- M.lookup b bToC
  return $ (a, c)

\end{code}

\end{document}
