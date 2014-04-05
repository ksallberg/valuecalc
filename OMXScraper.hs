module OMXScraper
(
     Ticker
   , Company(..)
   , parseOMX
   , toMilSek
   , toBilSek
   , fromCommanotation
)
where

import Control.Monad
import Control.Monad.Error
import Data.Either
import Scraping
import Text.HTML.TagSoup
import Network.HTTP

-- Error message for mcap
mcapError :: ErrorM
mcapError = "Error reading the OMX market cap."

-- Error message for total assets
asstError :: ErrorM
asstError = "Error reading OMX total assets."

-- Error message for liabilities
liabError :: ErrorM
liabError = "Error reading OMX total liabilities."

-- give the balance sheet url, the one to get assets and liabilites from
balanceSheetURL :: Ticker -> String
balanceSheetURL tick
   = "http://se.investing.com/equities/" ++ tick ++ "-balance-sheet"

toMilSek :: String -> Integer
toMilSek inp = read (inp ++ (take 6 (repeat '0'))) :: Integer

toBilSek :: String -> Integer
toBilSek inp = read (fromCommanotation inp) :: Integer

fromCommanotation :: String -> String
fromCommanotation inp =
   takeWhile (/=',') inp ++ follow ++ take (9-length follow) (repeat '0')
      where follow = tail $ takeWhile (/='B') $ dropWhile (/=',') inp

-- give the market url, the one to get 
marketURL :: Ticker -> String
marketURL tick = "http://se.investing.com/equities/" ++ tick

{-
   Given a URL, load the content and look for
   some predefined datapoints

   link is defined as the value from the balance sheet
   (total assets - total liabilities)

   markedLink is defined as the markets valuation of
   the company i.e. the market cap
-}
parseOMX :: Ticker -> ErrorW Company
parseOMX ticker =
   -- get the ticker from the url 
   do let link       = balanceSheetURL ticker
          marketLink = marketURL ticker
      reducedLs   <- getFromHTTP link
      totalAssets <- getTotalAssets reducedLs
      totalLiab   <- getTotalLiabilities reducedLs
      marketCap   <- getMarketCap marketLink
      return $ Company{name             = ticker,
                       totalAssets      = toMilSek totalAssets,
                       totalLiabilities = toMilSek totalLiab,
                       marketCap        = toBilSek marketCap}

{-
   OMX specific:
   From a list of tags, find the total
   assets value for the given company
-}
getTotalAssets :: [Tag String] -> ErrorW String
getTotalAssets t = getData t "Totalatillg\195\165ngar" 6 asstError

{-
   OMX specific:
   Parse total liabilities
-}
getTotalLiabilities :: [Tag String] -> ErrorW String
getTotalLiabilities t = getData t "Summaskulder" 6 liabError

{-
   OMX specific:
   Get the market cap from another page than the other
   data is fetched from
-}
getMarketCap :: String -> ErrorW String
getMarketCap link =
   getFromHTTP link >>= (\x->getData x "B\195\182rsv\195\164rde" 3 mcapError)