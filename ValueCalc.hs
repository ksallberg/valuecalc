module ValueCalc (
   main
) where

import Scraping
import OMXScraper
import NasdaqScraper
import Calculations
import NasdaqList
import OMXList
import Control.Monad
import Control.Monad.Error
import Data.Either

{-
   For now, 
-}
main :: IO ()
main = do loadCompanyList parseOMX    omxList
          putStrLn ""
          putStrLn "-end of omx, now nasdaq-"
          loadCompanyList parseNasdaq nasdaqList
          putStrLn ""
          putStrLn "-end-"

{-
   Determine if the company is undervalued,
   get the difference, and also print the
   data used to it's possible to manually
   confirm the result.

   Using Either to wrap the content.
-}
calcAndPrint :: Either String Company -> IO ()
calcAndPrint input = do
   case input of
      Left error -> putStr (show error)
      Right info -> do
         putStrLn ""
         putStr $ "name: "                ++ (name info)
         putStr $ ", total assets: "      ++ show (totalAssets info)
         putStr $ ", total liabilities: " ++ show (totalLiabilities info)
         putStr $ ", market cap: "   ++ show (marketCap info)
         putStr $ ", undervalued: "  ++ show (undervalued) ++
                  ", difference: "   ++ show (difference)

         where undervalued = isUnderValued (totalAssets info)
                                           (totalLiabilities info)
                                           (marketCap info)
               difference  = getDiff       (totalAssets info)
                                           (totalLiabilities info)
                                           (marketCap info)

{-
   For a list of given tickers, load the wanted data
   from some data sources, and give them to calcAndPrint
-}
loadCompanyList :: (Ticker -> ErrorW Company) -> [Ticker] -> IO ()
loadCompanyList scraper xs =
   forM_ xs $ \x->runErrorT (scraper x) >>= \res->calcAndPrint res
