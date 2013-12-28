module Scraper
(
     Ticker
   , Company(..)
   , parse
)
where

import Network.HTTP
import Text.HTML.TagSoup

type Ticker = String

-- give the balance sheet url, the one to get assets and liabilites from
balanceSheetURL :: Ticker -> String
balanceSheetURL tick =
   "http://stockreports.nasdaq.edgar-online.com/"++tick++".html"

-- give the market url, the one to get 
marketURL :: Ticker -> String
marketURL tick =
   "http://www.nasdaq.com/symbol/"++tick

-- From all TagText objects, remove all white spaces
dropWhitespace :: Tag String -> Tag String
dropWhitespace (TagText str) = (TagText (unwords (words str)))
dropWhitespace x             = x

-- Drop all TagText objects, which are only wrapping an empty string
dropEmpty :: [Tag String] -> [Tag String]
dropEmpty []                = []
dropEmpty ((TagText ""):xs) = [] ++ dropEmpty xs
dropEmpty (x:xs)            = x : dropEmpty xs

-- Convert from million dollar string
-- to dollar int
fromMilDol :: String -> Integer
fromMilDol str =
   read ([x|x<-takeWhile (\x->x/='.') str,x/=',']
         ++(take 6 (repeat '0'))) :: Integer

-- Convert from dollar sign and commas to just an Int
fromDolSign :: String -> Integer
fromDolSign str = read (drop 2 [x|x<-str,x/=',']) :: Integer

data Company = Company {
      
      name             :: String,
      totalAssets      :: Integer,
      totalLiabilities :: Integer,
      marketCap        :: Integer

   } deriving (Show,Read)

{-
   Given a URL, load the content and look for
   some predefined datapoints

   link is defined as the value from the balance sheet
   (total assets - total liabilities)

   markedLink is defined as the markets valuation of
   the company i.e. the market cap
-}
parse :: Ticker -> IO Company
parse ticker =
      -- get the ticker from the url 
   do let link       = balanceSheetURL ticker
          marketLink = marketURL ticker
      reducedLs   <- getFromHTTP link
      totalAssets <- getTotalAssets reducedLs
      totalLiab   <- getTotalLiabilities reducedLs
      marketCap   <- getMarketCap marketLink
      return Company{name             = ticker,
                     totalAssets      = fromMilDol  totalAssets,
                     totalLiabilities = fromMilDol  totalLiab,
                     marketCap        = fromDolSign marketCap}

{-
   Load content from HTTP
-}
getFromHTTP :: String -> IO [Tag String]
getFromHTTP link =
   do http <- simpleHTTP (getRequest link) >>= getResponseBody
      let tags = parseTags http
      return $ dropEmpty (map dropWhitespace tags)

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

{-
   Get the market cap from another page than the other
   data is fetched from
-}
getMarketCap :: String -> IO String
getMarketCap li = getFromHTTP li >>= (\x->getData x "Market cap" 13)
