module Analysis.Liveness.Register(RangeInfo,
                                  liveRange,
                                  liveRanges,
                                  rangesOverlap) where

import Data.List as L
import Data.Map as M

import Core.MiniSyntax
import Utils.MapUtils

data RangeInfo
  = RangeInfo (Map String (Int, Int))
    deriving (Eq, Ord, Show)

rangeInfo m = RangeInfo m

liveRange :: String -> RangeInfo -> (Int, Int)
liveRange regName (RangeInfo m) =
  lookupF regName m

liveRanges :: [Statement a] -> RangeInfo
liveRanges stmts =
  let namesUsedByIndex = L.zip (L.map namesReferenced stmts) [0..(length stmts - 1)]
      startMap = L.foldl addStartingNames M.empty namesUsedByIndex
      endMap = L.foldl addStartingNames M.empty $ L.reverse namesUsedByIndex in
  rangeInfo $ mergeKeys startMap endMap

addStartingNames :: Map String Int -> ([String], Int) -> Map String Int
addStartingNames namesSoFar (newNames, ind) =
  L.foldl (addNewNameAt ind) namesSoFar newNames

addNewNameAt :: Int -> Map String Int -> String -> Map String Int
addNewNameAt ind namesSoFar name =
  case M.lookup name namesSoFar of
    Just n -> namesSoFar
    Nothing -> M.insert name ind namesSoFar

rangesOverlap (l1, r1) (l2, r2) =
  case r1 < l2 of
    True -> False
    False -> case r2 < l1 of
      True -> False
      False -> True
