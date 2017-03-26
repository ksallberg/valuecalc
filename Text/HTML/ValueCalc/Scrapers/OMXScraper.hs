module Text.HTML.ValueCalc.Scrapers.OMXScraper
       (
         Ticker
       , Company(..)
       , parseOMX
       , toMilSek
       , toBilSek
       , fromZeros
       , prepare
       )
       where

import Control.Monad
import Control.Monad.Error
import Data.Either
import Data.Maybe
import Text.HTML.ValueCalc.Scraping
import Text.HTML.TagSoup
import Text.Read
import Network.HTTP

-- Error message for mcap
mcapError :: ErrorM
mcapError = "Error reading the OMX market cap."

mcapErrorR :: ErrorM
mcapErrorR = "Error reading refined market cap to Integer."

-- Error message for total assets
asstError :: ErrorM
asstError = "Error reading OMX total assets."

asstErrorR :: ErrorM
asstErrorR = "Error reading refined total assets to Integer."

-- Error message for liabilities
liabError :: ErrorM
liabError = "Error reading OMX total liabilities."

liabErrorR :: ErrorM
liabErrorR = "Error reading refined total liabilities to Integer."

-- give the balance sheet url, the one to get assets and liabilites from
balanceSheetURL :: Ticker -> String
balanceSheetURL tick
  -- = "http://se.investing.com/equities/" ++ tick ++ "-balance-sheet"
  = "http://quotes.morningstar.com/stockq/c-financials?&t=XSTO:" ++ tick

toMilSek :: String -> Maybe Integer
toMilSek inp = readMaybe (fromZeros ((prepare inp) ++ "B") 6 '.' 'B')

toBilSek :: String -> Maybe Integer
toBilSek inp = readMaybe (fromZeros ((prepare inp) ++ "B") 9 '.' 'B')

prepare :: String -> String
prepare inp = addPoint stage1
  where stage1 = takeWhile (/= 'b') ([x | x <- inp, x /= ','])

addPoint :: String -> String
addPoint noComma =
  case elem '.' noComma of
    True ->
      noComma
    False ->
      noComma ++ ".0"

fromZeros :: String -> Int -> Char -> Char -> String
fromZeros inp zer delim end =
  dropWhile (=='0') $ takeWhile c inp ++ follow ++ take lfoll (repeat '0')
  where follow = tail $ takeWhile (/=end) $ dropWhile c inp
        lfoll  = zer - length follow
        c      = (/=delim)

-- give the market url, the one to get
marketURL :: Ticker -> String
marketURL tick
  = "http://quotes.morningstar.com/stockq/c-header?&t=XSTO:" ++ tick

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
     -- handle the potential failure of reading a string to integer:
     totAssets   <- sFromJust (toMilSek totalAssets) asstErrorR
     totLiab     <- sFromJust (toMilSek totalLiab)   liabErrorR
     marketC     <- sFromJust (toBilSek marketCap)   mcapErrorR
     return $ Company{name             = ticker,
                      totalAssets      = totAssets,
                      totalLiabilities = totLiab,
                      marketCap        = marketC}

-- special from just, return or throw error inside the ErrorW monad
-- TODO: Is it possible to somehow do this using liftM or lift?
sFromJust :: Maybe Integer -> ErrorM -> ErrorW Integer
sFromJust Nothing  err = throwError err
sFromJust (Just x) err = return x

{-
   OMX specific:
   From a list of tags, find the total
   assets value for the given company
-}
getTotalAssets :: [Tag String] -> ErrorW String
getTotalAssets t = getData t "TotalAssets" 6 asstError

{-
   OMX specific:
   Parse total liabilities
-}
getTotalLiabilities :: [Tag String] -> ErrorW String
getTotalLiabilities t = getData t "TotalLiabilities" 6 liabError

{-
   OMX specific:
   Get the market cap from another page than the other
   data is fetched from
-}
getMarketCap :: String -> ErrorW String
getMarketCap link = do
  t <- getFromHTTP link
  getSubId t "MarketCap" mcapError
