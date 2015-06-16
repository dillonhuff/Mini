module Reporting.Plot(GChart, barChart, gChartToSVG, simpleBar) where

import Control.Monad
import Data.List as L
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

data GChart l x y = GChart {
  plotName :: String,
  chart :: EC l (Plot x y)
  }

gChart n c = GChart n c

idBarChart :: String -> [String] -> [(Int, [Double])] -> GChart l Int Double
idBarChart name seriesNames dataPts =
  gChart name $ liftM (plotBars) $ bars seriesNames $ L.map (\(x, y) -> (x, y)) dataPts

barChart :: String -> String -> [(Int, Int)] -> GChart l Int Int
barChart name seriesName dataPts =
  gChart name $ liftM (plotBars) $ bars [seriesName] $ L.map (\(x, y) -> (x, [y])) dataPts

gChartToSVG :: String -> GChart (Layout Int Int) Int Int -> IO ()
gChartToSVG filePath ct =
  toFile def (filePath ++ "/" ++ plotName ct ++ ".png") $ plot $ chart ct

gChartToPNG :: String -> GChart (Layout Int Double) Int Double -> IO ()
gChartToPNG filePath ct =
  toFile def (filePath ++ "/" ++ plotName ct ++ ".png") $ plot $ chart ct

simpleBar :: String -> String -> [String] -> [(String, [Double])] -> IO ()
simpleBar filePath chartTitle titles values = toFile def filePath $ do
    layout_title .= chartTitle
    layout_title_style . font_size .= 10
    layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
    plot $ fmap plotBars $ bars titles (addIndexes (map snd values))
