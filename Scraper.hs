module Scraper
(
     Company(..)
   , parse
)
where

import Network.HTTP
import Text.HTML.TagSoup

-- From all TagText objects, remove all white spaces
dropWhitespace :: Tag String -> Tag String
dropWhitespace (TagText str) = (TagText (unwords (words str)))
dropWhitespace x             = x

-- Drop all TagText objects, which are only wrapping an empty string
dropEmpty :: [Tag String] -> [Tag String]
dropEmpty []                = []
dropEmpty ((TagText ""):xs) = [] ++ dropEmpty xs
dropEmpty (x:xs)            = x : dropEmpty xs

data Company = Company {
      
      name             :: String,
      totalAssets      :: String,
      totalLiabilities :: String

   } deriving (Show,Read)

{-
   Given a URL, load the content and look for
   some predefined datapoints

   TODO: maybe give a list of datapoints
         to look for
-}
parse :: String -> IO Company
parse link =
      -- get the ticker from the url 
   do let ticker = reverse $ takeWhile (\y->y/='/') 
                             (tail $ dropWhile (\x->x/='.') (reverse link))
      http <- simpleHTTP (getRequest link) >>= getResponseBody
      let tags = parseTags http
          reducedLs = dropEmpty (map dropWhitespace tags)
      totalAssets <- getTotalAssets reducedLs
      totalLiab   <- getTotalLiabilities reducedLs
      return Company{name             = ticker,
                     totalAssets      = totalAssets,
                     totalLiabilities = totalLiab}

{-
   Get the name
-}
getCompanyName :: [Tag String] -> IO String
getCompanyName t = undefined

{-
   From a list of tags, find the total
   assets value for the given company
-}
getTotalAssets :: [Tag String] -> IO String
getTotalAssets t = getData t "Total Assets" 5

{-
   Parse total liabilities
-}
getTotalLiabilities :: [Tag String] -> IO String
getTotalLiabilities t = getData t "Total Liabilities" 5

{-
   Generalized function for finding the financial data
   searched for

   tags  = all tags to search in
   key   = the string (header/description) to search for
   index = the following tag that holds what we're looking for

   fTags = followingTags
-}
getData :: [Tag String] -> String -> Int -> IO String
getData tags key index =
   do let fTags              = dropWhile (~/= (TagText key)) tags
      let (TagText toReturn) = fTags !! index
      return toReturn
