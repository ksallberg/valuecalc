module OMXScraper
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

-- give the market url, the one to get 
marketURL :: Ticker -> String
marketURL = balanceSheetURL -- for now use the same as the balance sheet

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
      liftIO (putStrLn link)
      reducedLs   <- getFromHTTP link
--      liftIO $ putStrLn (show (take 10 reducedLs))
      totalAssets <- getTotalAssets reducedLs
      liftIO $ putStrLn (totalAssets)
      totalLiab   <- getTotalLiabilities reducedLs
      marketCap   <- getMarketCap marketLink
      return $ Company{name             = ticker,
                       totalAssets      = (read totalAssets) :: Integer,
                       totalLiabilities = (read totalLiab)   :: Integer,
                       marketCap        = (read marketCap)   :: Integer}

{-
   NASDAQ specific:
   From a list of tags, find the total
   assets value for the given company
-}
getTotalAssets :: [Tag String] -> ErrorW String
getTotalAssets t = getData t "Volvo" 2 asstError

{-
   NASDAQ specific:
   Parse total liabilities
-}
getTotalLiabilities :: [Tag String] -> ErrorW String
getTotalLiabilities t = getData t "Summa skulder" 5 liabError

{-
   NASDAQ specific:
   Get the market cap from another page than the other
   data is fetched from
-}
getMarketCap :: String -> ErrorW String
getMarketCap link =
   getFromHTTP link >>= (\x->getData x "Market cap" 13 mcapError)
