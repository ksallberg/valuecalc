module Main (
   main
) where

import Text.HTML.ValueCalc
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
      Left  error   -> putStr (show error)
      Right company -> do
         putStrLn ""
         putStr $ "name: "                ++ (name company)
         putStr $ ", total assets: "      ++ show (totalAssets company)
         putStr $ ", total liabilities: " ++ show (totalLiabilities company)
         putStr $ ", market cap: "   ++ show (marketCap company)
         putStr $ ", undervalued: "  ++ show (undervalued) ++
                  ", difference: "   ++ show (difference)
         where undervalued = isUnderValued company
               difference  = getDiff company

{-
   For a list of given tickers, load the wanted data
   from some data sources, and give them to calcAndPrint
-}
loadCompanyList :: (Ticker -> ErrorW Company) -> [Ticker] -> IO ()
loadCompanyList scraper xs =
   forM_ xs $ \x->runErrorT (scraper x) >>= \res->calcAndPrint res
