{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Collect.IEX
       (
         testEarnings,
         testCompany,
         testChart,
         testDQ,
         testDividend,
         testES,
         testFin,
         testStats,
         testNews,
         testOHLC
       ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
-- import           Network.HTTP.Types.Status
import           System.IO
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import           Data.Maybe
import           Text.Pretty.Simple (pPrint)

type IEXNumber = Double

data Chart = Chart {
  -- is only available on 1d chart.
  minute :: Maybe String,
  -- is only available on 1d chart. 15 minute delayed
  marketAverage :: Maybe IEXNumber,
  -- is only available on 1d chart. 15 minute delayed
  marketNotional :: Maybe IEXNumber,
  -- is only available on 1d chart. 15 minute delayed
  marketNumberOfTrades :: Maybe IEXNumber,
  -- is only available on 1d chart. 15 minute delayed
  marketHigh :: Maybe IEXNumber,
  -- is only available on 1d chart. 15 minute delayed
  marketLow :: Maybe IEXNumber,
  -- is only available on 1d chart. 15 minute delayed
  marketVolume :: Maybe IEXNumber,
  -- is only available on 1d chart. Percent change
  -- of each interval relative to first value. 15 minute delayed
  marketChangeOverTime :: Maybe IEXNumber,
  -- is only available on 1d chart.
  average :: Maybe IEXNumber,
  -- is only available on 1d chart.
  notional :: Maybe IEXNumber,
  -- is only available on 1d chart.
  numberOfTrades :: Maybe IEXNumber,
  -- is only available on 1d chart, and only when chartSimplify is true.
  -- The first element is the original number of points.
  -- Second element is how many remain after simplification.
  simplifyFactor :: Maybe [Integer],
  -- is available on all charts.
  high :: IEXNumber,
  -- is available on all charts.
  low :: IEXNumber,
  -- is available on all charts.
  volume :: Integer,
  -- is available on all charts. A variable formatted version of
  -- the date depending on the range. Optional convienience field.
  label :: String,
  -- is available on all charts. Percent change of each interval
  -- relative to first value. Useful for comparing multiple stocks.
  changeOverTime :: IEXNumber,
  -- is not available on 1d chart.
  date :: Maybe String,
  -- is not available on 1d chart.
  open :: Maybe IEXNumber,
  -- is not available on 1d chart.
  close :: Maybe IEXNumber,
  -- is not available on 1d chart.
  unadjustedVolume :: Maybe Integer,
  -- is not available on 1d chart.
  change :: Maybe IEXNumber,
  -- is not available on 1d chart.
  changePercent :: Maybe IEXNumber,
  -- is not available on 1d chart.
  vwap :: Maybe IEXNumber
} deriving (Generic, Show)

instance ToJSON Chart
instance FromJSON Chart

data Company = Company {
  symbol :: String,
  companyName :: String,
  exchange :: String,
  industry :: String,
  website :: String,
  description :: String,
  ceo :: String,
  issueType :: String,
  sector :: String
} deriving (Generic, Show)

customOptionsCompany = defaultOptions {
  fieldLabelModifier = let f "ceo" = "CEO"
                           f other = other
                       in f
  }

-- FromJSON means parsing the text into a haskell data structure
instance FromJSON Company where
  parseJSON = genericParseJSON customOptionsCompany
-- ToJSON means taking a haskell data structure and making a JSON string
instance ToJSON Company

data DelayedQuote = DelayedQuote {
  symbol :: String,
  delayedPrice :: IEXNumber,
  high :: IEXNumber,
  low :: IEXNumber,
  delayedSize :: IEXNumber,
  delayedPriceTime :: Integer,
  processedTime :: Integer
} deriving (Generic, Show)

instance FromJSON DelayedQuote
instance ToJSON DelayedQuote

data Dividend = Dividend {
  exDate :: String,
  paymentDate :: String,
  recordDate :: String,
  declaredDate :: String,
  amount :: IEXNumber,
  flag :: String,
  dtype :: String,
  qualified :: String,
  indicated :: String
} deriving (Generic, Show)

customOptionsDividend = defaultOptions {
  fieldLabelModifier = let f "dtype" = "type"
                           f other = other
                       in f
  }

-- FromJSON means parsing the text into a haskell data structure
instance FromJSON Dividend where
  parseJSON = genericParseJSON customOptionsDividend
-- ToJSON means taking a haskell data structure and making a JSON string
instance ToJSON Dividend

data Earning = Earning {
  actualEPS :: Double,
  consensusEPS :: Double,
  estimatedEPS :: Double,
  announceTime :: String,
  numberOfEstimates :: Int,
  epsSurpriseDollar :: Double,
  epsReportDate :: String,
  fiscalPeriod :: String,
  fiscalEndDate :: String
} deriving (Generic, Show)

data Earnings = Earnings {
  symbol :: String,
  earnings :: [Earning]
} deriving (Generic, Show)

instance ToJSON Earnings
instance FromJSON Earnings

customOptions = defaultOptions {
  fieldLabelModifier = let f "epsSurpriseDollar" = "EPSSurpriseDollar"
                           f "epsReportDate" = "EPSReportDate"
                           f other = other
                       in f
  }

-- FromJSON means parsing the text into a haskell data structure
instance FromJSON Earning where
  parseJSON = genericParseJSON customOptions
-- ToJSON means taking a haskell data structure and making a JSON string
instance ToJSON Earning

data EffectiveSpread = EffectiveSpread {
  volume :: Integer,
  venue :: String,
  venueName :: String,
  effectiveSpread :: IEXNumber,
  effectiveQuoted :: IEXNumber,
  priceImprovement :: IEXNumber
} deriving (Generic, Show)

instance ToJSON EffectiveSpread
instance FromJSON EffectiveSpread

data Financial = Financial {
  reportDate :: String,
  grossProfit :: Integer,
  costOfRevenue :: Integer,
  operatingRevenue :: Integer,
  totalRevenue :: Integer,
  operatingIncome :: Integer,
  netIncome :: Integer,
  researchAndDevelopment :: Integer,
  operatingExpense :: Integer,
  currentAssets :: Integer,
  totalAssets :: Integer,
  totalLiabilities :: Maybe Integer,
  currentCash :: Integer,
  currentDebt :: Integer,
  totalCash :: Integer,
  totalDebt :: Integer,
  shareholderEquity :: Integer,
  cashChange :: Integer,
  cashFlow :: Integer,
  operatingGainsLosses :: Maybe String
} deriving (Generic, Show)

instance ToJSON Financial
instance FromJSON Financial

data Financials = Financials {
  symbol :: String,
  financials :: [Financial]
} deriving (Generic, Show)

instance ToJSON Financials
instance FromJSON Financials

data Stats = Stats {
  companyName :: String,
  marketcap :: Integer,
  beta :: Double,
  week52high :: Double,
  week52low :: Double,
  week52change :: Double,
  shortInterest :: Integer,
  shortDate :: String,
  dividendRate :: Double,
  dividendYield :: Double,
  exDividendDate :: String,
  latestEPS :: Double,
  latestEPSDate :: String,
  sharesOutstanding :: Integer,
  float :: Integer,
  returnOnEquity :: Double,
  consensusEPS :: Double,
  numberOfEstimates :: Integer,
  -- EPSSurpriseDollar
  -- EPSSurprisePercent
  symbol :: String,
  -- EBITDA
  revenue :: Integer,
  grossProfit :: Integer,
  cash :: Integer,
  debt :: Integer,
  ttmEPS :: Double,
  revenuePerShare :: Integer,
  revenuePerEmployee :: Integer,
  peRatioHigh :: Double,
  peRatioLow :: Double,
  returnOnAssets :: Double,
  returnOnCapital :: Maybe Double,
  profitMargin :: Double,
  priceToSales :: Double,
  priceToBook :: Double,
  day200MovingAvg :: Double,
  day50MovingAvg :: Double,
  institutionPercent :: Double,
  insiderPercent :: Maybe Double,
  shortRatio :: Maybe Double,
  year5ChangePercent :: Double,
  year2ChangePercent :: Double,
  year1ChangePercent :: Double,
  ytdChangePercent :: Double,
  month6ChangePercent :: Double,
  month3ChangePercent :: Double,
  month1ChangePercent :: Double,
  day5ChangePercent :: Double,
  day30ChangePercent :: Double
} deriving (Generic, Show)

instance ToJSON Stats
instance FromJSON Stats

data NewsItem = NewsItem {
  datetime :: String,
  headline :: String,
  source :: String,
  url :: String,
  summary :: String,
  related :: String
} deriving (Generic, Show)

instance ToJSON NewsItem
instance FromJSON NewsItem

data OHLC = OHLC {
  open :: PriceTime,
  close :: PriceTime,
  high :: Double,
  low :: Double
} deriving (Generic, Show)

instance ToJSON OHLC
instance FromJSON OHLC

data PriceTime = PriceTime {
  price :: Double,
  time :: Integer
} deriving (Generic, Show)

instance ToJSON PriceTime
instance FromJSON PriceTime

base :: String
base = "https://api.iextrading.com/1.0"

testChart :: IO ()
testChart = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/chart"
  pPrint (fromJust (decode json :: Maybe [Chart]))

testCompany :: IO ()
testCompany = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/company"
  pPrint (fromJust (decode json :: Maybe Company))

testDQ :: IO ()
testDQ = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/delayed-quote"
  pPrint (fromJust (decode json :: Maybe DelayedQuote))

testDividend :: IO ()
testDividend = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/dividends/5y"
  pPrint (fromJust (decode json :: Maybe [Dividend]))

testEarnings :: IO ()
testEarnings = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/earnings"
  pPrint (fromJust (decode json :: Maybe Earnings))

testES :: IO ()
testES = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/effective-spread"
  pPrint (fromJust (decode json :: Maybe [EffectiveSpread]))

testFin :: IO ()
testFin = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/financials"
  pPrint (fromJust (decode json :: Maybe Financials))

testStats :: IO ()
testStats = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/stats"
  pPrint (fromJust (decode json :: Maybe Stats))

testNews :: IO ()
testNews = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/news/last/1"
  pPrint (fromJust (decode json :: Maybe [NewsItem]))

testOHLC :: IO ()
testOHLC = do
  manager  <- newManager tlsManagerSettings
  json <- fetch manager "/stock/aapl/ohlc"
  pPrint (fromJust (decode json :: Maybe OHLC))

fetch :: Manager -> String -> IO (L8.ByteString)
fetch manager path = do
  request  <- parseRequest (base ++ path)
  response <- httpLbs request manager
  return (responseBody response :: L8.ByteString)
