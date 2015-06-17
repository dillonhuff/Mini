module Reporting.Utils(writeHtmlToFile,
             chartHtml,
             dateTimeString,
             normalizeString,
             stringListToHtml,
             stringListToHyperLinkList) where

import Control.Monad
import Data.List as L
import Data.Time
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String

writeHtmlToFile :: FilePath -> Html -> IO ()
writeHtmlToFile fileName htmlText =
  writeFile (fileName ++ ".html") (renderHtml htmlText)

chartHtml :: String -> String -> Html
chartHtml imgPath imgAltTagText =
  img ! src (toValue ("charts/" ++ imgPath ++ ".png")) ! alt (toValue imgAltTagText)

stringListToHtml :: String -> [String] -> Html
stringListToHtml title items = body $ do
  p $ toHtml title
  ul $ forM_ items (li . toHtml)

stringListToHyperLinkList :: String -> [String] -> Html
stringListToHyperLinkList title items = body $ do
  p $ toHtml title
  ul $ forM_ items (\linkName -> li $ a ! href (toValue $ linkName ++ ".html") $ toHtml linkName)

dateTimeString :: IO String
dateTimeString = liftM (normalizeString . show) $ getZonedTime

normalizeString :: String -> String
normalizeString str = L.map normalizeChar str

normalizeChar :: Char -> Char
normalizeChar ' ' = '_'
normalizeChar ':' = '-'
normalizeChar '.' = '-'
normalizeChar '/' = '_'
normalizeChar c = c
