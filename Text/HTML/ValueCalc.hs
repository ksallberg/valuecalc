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
   isUnderValued,
   omxList,
   nasdaqList,
   parseOMX,
   parseNasdaq,
   ErrorW,
   Ticker
) where

import Text.HTML.ValueCalc.Calculations
import Text.HTML.ValueCalc.Scraping
import Text.HTML.ValueCalc.Lists.NasdaqList
import Text.HTML.ValueCalc.Lists.OMXList
import Text.HTML.ValueCalc.Scrapers.NasdaqScraper
import Text.HTML.ValueCalc.Scrapers.OMXScraper
