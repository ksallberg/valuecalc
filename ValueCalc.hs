module ValueCalc (
   main
) where

import Scraper
import Control.Monad

main :: IO ()
main =
   do loadCompanyList ["mdca","msft"]
      putStrLn "----exiting----"

edgarLink :: String
edgarLink = "http://stockreports.nasdaq.edgar-online.com/"

printCompany :: IO Company -> IO ()
printCompany comp =
   do info <- comp
      putStrLn "_____________"
      putStrLn $ "name: " ++ (name info)
      putStrLn $ "total assets: " ++ show (totalAssets info)
      putStrLn $ "total liabilities: " ++ show (totalLiabilities info)

loadCompanyList :: [String] -> IO ()
loadCompanyList ls =
   do let ps = [parse (edgarLink++x++".html")|x<-ls]
      forM_ ps printCompany
