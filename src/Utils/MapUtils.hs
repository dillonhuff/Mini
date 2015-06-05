module Utils.MapUtils(chain) where

import Data.List as L
import Data.Map as M
import Data.Maybe

chain :: (Ord a, Ord b) => Map a b -> Map b c -> Map a c
chain aToB bToC =
  let aToBPairs = M.toList aToB
      maybeAToCPairs = L.map (\(a, b) -> maybeNewPair a b bToC) aToBPairs in
  M.fromList $ catMaybes maybeAToCPairs

maybeNewPair :: (Ord b) => a -> b -> Map b c -> Maybe (a, c)
maybeNewPair a b bToC = do
  c <- M.lookup b bToC
  return $ (a, c)
