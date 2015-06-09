module Utils.MapUtils(chain,
                      lookupF,
                      mergeKeys) where

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

lookupF k m =
  case M.lookup k m of
    Just v -> v
    Nothing -> error $ "lookupF: Cannot find " ++ show k ++ " in " ++ show m

mergeKeys :: (Show a, Show c, Ord a) => Map a b -> Map a c -> Map a (b, c)
mergeKeys l r =
  M.fromList $ L.map (\(k, v) -> (k, (v, lookupF k r))) $ M.toList l
