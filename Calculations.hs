module Calculations
(
     getDiff,
     isUnderValued
)
where

{-
   How much undervalued is the company? If negative
   value, then the company is instead overvalued...
-}
getDiff :: Int -> Int -> Int -> Int
getDiff totAsst totLia marketCap = 
   (totAsst-totLia) - marketCap

{-
   If the value from the balance sheet
   is larger than the market valuation,
   then the company is undervalued
-}
isUnderValued :: Int -> Int -> Int -> Bool
isUnderValued totAsst totLia marketCap =
   (totAsst - totLia) > marketCap
