module Utils.GenerateRandomValues(assignRandomValuesInRange) where

import Control.Monad.Random
import Data.List as L
import Data.Map as M

assignRandomValuesInRange :: (Random a, Ord b, MonadRandom m) => a -> a -> [b] -> m (Map b a)
assignRandomValuesInRange lo hi l = do
  randVals <- getRandomRs (lo, hi)
  return $ M.fromList $ L.zip l (L.take (length l) randVals)
