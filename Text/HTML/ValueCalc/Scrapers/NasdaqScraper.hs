{-# LANGUAGE PatternGuards #-}

module Text.HTML.ValueCalc.Scrapers.NasdaqScraper
(
     Ticker
   , Company(..)
   , parseNasdaq
   , fromMilDol
   , fromDolSign
)
where

import Data.Either
import Text.HTML.ValueCalc.Scraping

-- give the balance sheet url, the one to get assets and liabilites from
balanceSheetURL :: Ticker -> String
balanceSheetURL tick =
   concat ["http://stockreports.nasdaq.edgar-online.com/", tick, ".html"]

-- give the market url, the one to get
marketURL :: Ticker -> String
marketURL tick = "http://www.nasdaq.com/symbol/"++tick

-- Convert from million dollar string
-- to dollar int (million dollars -> dollars)
{-
   a string represented number that is expressing million dollars.

   take all chars before . and remove , from this = beforeCommaNoDots

   after that just add six 0 to represent it in dollars
-}
fromMilDol :: String -> Integer
fromMilDol ""  = 0
                 -- keeping this to make sure we can still handle
                 -- . as the first input
fromMilDol str | head str == '.' = fromMilDol ("0"++str)
fromMilDol str | length [x|x<-str, x /= ',' && x /= '.'] == 0 = 0
fromMilDol str = d * 1000000
   where d = read (takeWhile p $ filter n str) :: Integer
         n = (/=',')
         p = (/='.')

-- Convert from dollar sign and commas to just an Int
fromDolSign :: String -> Integer
fromDolSign "$" = 0 -- if there's no value in the table, then don't read
fromDolSign str = read (drop 2 [x|x<-str,x/=',']) :: Integer

{-
   Given a URL, load the content and look for
   some predefined datapoints

   link is defined as the value from the balance sheet
   (total assets - total liabilities)

   markedLink is defined as the markets valuation of
   the company i.e. the market cap
-}
parseNasdaq :: Ticker -> ErrorW Company
parseNasdaq ticker =
   -- get the ticker from the url
   do let link       = balanceSheetURL ticker
          marketLink = marketURL ticker
      reducedLs   <- getFromHTTP link
      totalAssets <- getTotalAssets reducedLs
      totalLiab   <- getTotalLia    reducedLs
      marketCap   <- getMarketCap marketLink
      return $ Company{name             = ticker,
                       totalAssets      = fromMilDol  totalAssets,
                       totalLiabilities = fromMilDol  totalLiab,
                       marketCap        = fromDolSign marketCap}

{-
   NASDAQ specific:
   From a list of tags, find the total assets value for the given company
-}
getTotalAssets :: [Tag String] -> ErrorW String
getTotalAssets t = getData t "TotalAssets" 5 "Error reading total assets"

{-
   NASDAQ specific:
   Parse total liabilities
-}
getTotalLia :: [Tag String] -> ErrorW String
getTotalLia t = getData t "TotalLiabilities" 5 "Error reading total liab"

{-
   NASDAQ specific:
   Get the market cap from another page than the other data is fetched from
-}
getMarketCap :: String -> ErrorW String
getMarketCap link = getFromHTTP link >>=
                    (\x->getData x "Marketcap" 13 "Error reading market cap")
