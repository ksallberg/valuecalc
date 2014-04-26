module Calculations
(
     getDiff,
     isUnderValued
)
where

import Scraping (Company (..))

{-
   How much undervalued is the company? If negative
   value, then the company is instead overvalued...
-}
getDiff :: Company -> Integer
getDiff company = 
   ((totalAssets company)-(totalLiabilities company)) - (marketCap company)

{-
   If the value from the balance sheet
   is larger than the market valuation,
   then the company is undervalued
-}
isUnderValued :: Company -> Bool
isUnderValued company =
   ((totalAssets company)-(totalLiabilities company)) > (marketCap company)
