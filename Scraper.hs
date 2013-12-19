module Scraper
(
     Company(..)
   , parse
)
where

import Network.HTTP
import Text.HTML.TagSoup

totAsst :: Tag String
totAsst = TagOpen "TR" [("STYLE","background-color: #CCFFCC")]

asst :: Tag String
asst = TagText "Total Assets"

data Company = Company {
      
      name :: String,
      totalAssets :: String

   } deriving (Show)
   
{-
   Given a URL, load the content and look for
   some predefined datapoints

   TODO: maybe give a list of datapoints
         to look for
-}
parse :: String -> IO Company
parse x = do
    http <- simpleHTTP (getRequest x) >>= getResponseBody
    let tags = parseTags http
        tota = dropWhile (~/= asst) tags
        (TagText totAssets) = (take 20 tota) !! 14
        parsedTotAssets = read totAssets :: Int

    return Company{name="mdca", totalAssets=totAssets}
