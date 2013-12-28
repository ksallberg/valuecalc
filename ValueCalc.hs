module ValueCalc (
   main
) where

import KParser
import Scraper

{-
   use the loadMDCA function get data points
   from the edgar website

   TODO: read the data properly and pass 
         the information to some calculation module
-}
main :: IO ()
main =
   do loadMDCA
      putStrLn "exiting"

loadMDCA :: IO ()
loadMDCA =
   do info <- parse "http://stockreports.nasdaq.edgar-online.com/msft.html"
      putStrLn $ "name: " ++ (name info)
      putStrLn $ "total assets: " ++ show (totalAssets info)
      putStrLn $ "total liabilities: " ++ show (totalLiabilities info)
