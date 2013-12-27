module Scraper
(
     Company(..)
   , parse
)
where

import Network.HTTP
import Text.HTML.TagSoup
import Control.Monad

--totAsst :: Tag String
--totAsst = TagOpen "td" [("width", "32%")]

asst :: Tag String
asst = TagText "Total Assets"


dropWhitespace :: Tag String -> Tag String
dropWhitespace (TagText str) = (TagText (unwords (words str)))
dropWhitespace x = x

-- totAsstText :: Tag String
-- totAsstText = TagText "T"

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
        tota = dropWhile (~/= asst) (map dropWhitespace tags)
    putStrLn $ "amount" ++ (show tota)
    forM_ tota (\x->putStrLn $ "hmm:" ++ show x)
    let (TagText totAssets) = (take 20 tota) !! 14
        parsedTotAssets = read totAssets :: Int

    return Company{name="mdca", totalAssets=totAssets}
