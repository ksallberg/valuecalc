module ValueCalc (
   main
) where

import Scraper
import Calculations
import Nasdaq
import Control.Monad
import Data.Either

{-
   Define a list of stocks to examine
   and perform the calculations!
-}
main :: IO ()
main =
   do loadCompanyList nasdaqList
      putStrLn "----no more stocks in the list----"

{-
   Determine if the company is undervalued,
   get the difference, and also print the
   data used to it's possible to manually
   confirm the result.

   Using Either to wrap the content.
-}
calcAndPrint :: IO (Either String Company) -> IO ()
calcAndPrint input =
   do comp <- input
      case comp of
         Left error -> putStrLn $ "Error parsing; " ++ error
         Right info -> do
            let undervalued = isUnderValued (totalAssets info)
                                            (totalLiabilities info)
                                            (marketCap info)
                difference  = getDiff (totalAssets info)
                                      (totalLiabilities info)
                                      (marketCap info)
            putStrLn "_____________"
            putStr $ "name: "                ++ (name info)
            putStr $ ", total assets: "      ++ show (totalAssets info)
            putStr $ ", total liabilities: " ++ show (totalLiabilities info)
            putStr $ ", market cap: "   ++ show (marketCap info)
            putStrLn "" -- new line
            putStrLn $ "undervalued: "  ++ show (undervalued) ++
                       ", difference: " ++ show (difference)

{-
   For a list of given tickers, load the wanted data
   from some data sources, and give them to calcAndPrint
-}
loadCompanyList :: [Ticker] -> IO ()
loadCompanyList ls = forM_ [parse x|x<-ls] calcAndPrint
