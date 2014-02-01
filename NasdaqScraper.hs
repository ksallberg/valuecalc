module NasdaqScraper
(
     Ticker
   , Company(..)
   , parse
)
where

import Data.Either
import Scraping

-- Error message for mcap
mcapError :: ErrorM
mcapError = "Error reading the market cap."

-- Error message for total assets
asstError :: ErrorM
asstError = "Error reading total assets."

-- Error message for liabilities
liabError :: ErrorM
liabError = "Error reading total liabilities."

-- give the balance sheet url, the one to get assets and liabilites from
balanceSheetURL :: Ticker -> String
balanceSheetURL tick =
   "http://stockreports.nasdaq.edgar-online.com/"++tick++".html"

-- give the market url, the one to get 
marketURL :: Ticker -> String
marketURL tick = "http://www.nasdaq.com/symbol/"++tick

-- Convert from million dollar string
-- to dollar int
fromMilDol :: String -> Integer
fromMilDol str =
   read ([x|x<-takeWhile (\x->x/='.') str,x/=',']
         ++(take 6 (repeat '0'))) :: Integer

-- Convert from dollar sign and commas to just an Int
fromDolSign :: String -> Integer
fromDolSign "$" = 0 -- sometimes there's no value in the table,
                    -- then don't read it :)
fromDolSign str = read (drop 2 [x|x<-str,x/=',']) :: Integer

{-
   Given a URL, load the content and look for
   some predefined datapoints

   link is defined as the value from the balance sheet
   (total assets - total liabilities)

   markedLink is defined as the markets valuation of
   the company i.e. the market cap
-}
parse :: Ticker -> ErrorW Company
parse ticker =
   -- get the ticker from the url 
   do let link       = balanceSheetURL ticker
          marketLink = marketURL ticker
      reducedLs   <- getFromHTTP link
      totalAssets <- getTotalAssets reducedLs
      totalLiab   <- getTotalLiabilities reducedLs
      marketCap   <- getMarketCap marketLink
      return $ Company{name             = ticker,
                       totalAssets      = fromMilDol  totalAssets,
                       totalLiabilities = fromMilDol  totalLiab,
                       marketCap        = fromDolSign marketCap}

{-
   NASDAQ specific:
   From a list of tags, find the total
   assets value for the given company
-}
getTotalAssets :: [Tag String] -> ErrorW String
getTotalAssets t = getData t "Total Assets" 5 asstError

{-
   NASDAQ specific:
   Parse total liabilities
-}
getTotalLiabilities :: [Tag String] -> ErrorW String
getTotalLiabilities t = getData t "Total Liabilities" 5 liabError

{-
   NASDAQ specific:
   Get the market cap from another page than the other
   data is fetched from
-}
getMarketCap :: String -> ErrorW String
getMarketCap link =
   getFromHTTP link >>= (\x->getData x "Market cap" 13 mcapError)
