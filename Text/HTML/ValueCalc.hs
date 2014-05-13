-- TODO: export the ValueCalc library
module Text.HTML.ValueCalc
(
   Company (..),
   dropWhitespace,
   dropEmpty,
   fromMilDol,
   fromDolSign,
   toMilSek,
   toBilSek,
   fromCommanotation,
   getDiff, 
   isUnderValued
) where

import Text.HTML.ValueCalc.Scraping
import Text.HTML.ValueCalc.Scrapers.NasdaqScraper
import Text.HTML.ValueCalc.Scrapers.OMXScraper
import Text.HTML.ValueCalc.Calculations
