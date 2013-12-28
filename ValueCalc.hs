module ValueCalc (
   main
) where

import Scraper
import Control.Monad

main :: IO ()
main =
   do loadCompanyList ["mdca","msft"]
      putStrLn "----exiting----"

printCompany :: IO Company -> IO ()
printCompany comp =
   do info <- comp
      putStrLn "_____________"
      putStrLn $ "name: " ++ (name info)
      putStrLn $ "total assets: " ++ show (totalAssets info)
      putStrLn $ "total liabilities: " ++ show (totalLiabilities info)
      putStrLn $ "market cap: " ++ show (marketCap info)

loadCompanyList :: [String] -> IO ()
loadCompanyList ls = forM_ [parse x|x<-ls] printCompany
