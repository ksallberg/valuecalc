{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Collect.IEX
       (
         testIEX
       ) where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
-- import           Network.HTTP.Types.Status
import           System.IO
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH

data Earning = Earning {
  actualEPS :: Double,
  consensusEPS :: Double,
  estimatedEPS :: Double,
  announceTime :: String,
  numberOfEstimates :: Int,
  epsSurpriseDollar :: String,
  -- epsReportDate :: String,
  fiscalPeriod :: String,
  fiscalEndDate :: String
} deriving (Show)

data Earnings = Earnings {
  symbol :: String,
  earnings :: [Earning]
} deriving (Generic, Show)

-- instance ToJSON Earning
instance ToJSON Earnings

$(deriveToJSON defaultOptions {
     fieldLabelModifier = let f "EPSSurpriseDollar" = "epsSurpriseDollar"
                              -- f "epsReportDate"     = "EPSReportDate"
                              f other               = other
                          in f
     } ''Earning)

$(deriveFromJSON defaultOptions {
     fieldLabelModifier = let f epsSurpriseDollar = "EPSSurpriseDollar"
                              -- f epsReportDate = "EPSReportDate"
                              f other = other
                          in f
     } ''Earning)

-- instance FromJSON Earning
instance FromJSON Earnings

base :: String
base = "https://api.iextrading.com/1.0"

testIEX :: IO ()
testIEX = do
  manager  <- newManager tlsManagerSettings
  printResponse manager "/stock/aapl/earnings"

printResponse :: Manager -> String -> IO ()
printResponse manager path = do
  request  <- parseRequest (base ++ path)
  response <- httpLbs request manager
  let str  = (responseBody response) :: L8.ByteString
      json = decode str :: Maybe Earnings
  putStrLn $ (L8.unpack str)
  putStrLn $ "____" ++ (show json)
