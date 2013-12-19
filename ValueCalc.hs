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
main = do putStrLn "Test parsing MDCA"
          loadMDCA
          putStrLn "exiting"

loadMDCA :: IO ()
loadMDCA = do info <- parse "http://www.sec.gov/Archives/edgar/data/876883/000114420412014991/v303734_10k.htm"
              putStrLn $ "name: " ++ (name info)
              putStrLn $ "total assets: " ++ show (totalAssets info)
