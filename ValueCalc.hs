module ValueCalc (
   main
) where

import Scraper
import Calculations
import Control.Monad

{-
   Define a list of stocks to examine
   and perform the calculations!
-}
main :: IO ()
main =
   do loadCompanyList ["mdca","msft"]
      putStrLn "----no more stocks in the list----"

{-
   Determine if the company is undervalued,
   get the difference, and also print the
   data used to it's possible to manually
   confirm the result.
-}
calcAndPrint :: IO Company -> IO ()
calcAndPrint comp =
   do info <- comp
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
