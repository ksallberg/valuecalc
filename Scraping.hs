{-
   Here I factor out functionality common for all scrapers.
   
   Re-exporting some of tagsoup's tags to hide them
   from the application specific parsers, for instance
   NASDAQ
-}

module Scraping
(
   getData,
   Ticker,
   ErrorM,
   ErrorW,
   Tag (..),
   Company (..),
   liftIO,
   parseTags,
   getFromHTTP
)

where

import Network.HTTP
import Control.Monad.Error
import Text.HTML.TagSoup

type Ticker = String
type ErrorM = String           -- Error Message
type ErrorW = ErrorT ErrorM IO -- Error Wrapper

{-
   Basic data structure containing everything I
   want to know about a company right now.
-}
data Company = Company {
      
      name             :: String,
      totalAssets      :: Integer,
      totalLiabilities :: Integer,
      marketCap        :: Integer

   } deriving (Show,Read)

-- From all TagText objects, remove all white spaces
dropWhitespace :: Tag String -> Tag String
dropWhitespace (TagText str) = (TagText (unwords (words str)))
dropWhitespace x             = x

-- Drop all TagText objects, which are only wrapping an empty string
dropEmpty :: [Tag String] -> [Tag String]
dropEmpty []                = []
dropEmpty ((TagText ""):xs) = [] ++ dropEmpty xs
dropEmpty (x:xs)            = x : dropEmpty xs

{-
   Load content from HTTP
-}
getFromHTTP :: String -> ErrorW [Tag String]
getFromHTTP link = do
   http <- liftIO $ simpleHTTP (getRequest link) >>= getResponseBody
   let tags = parseTags http
   return $ dropEmpty (map dropWhitespace tags)

{-
   Generalized function for finding the financial data
   searched for

   tags  = all tags to search in
   key   = the string (header/description) to search for
   index = the following tag that holds what we're looking for

   fTags = followingTags
-}
getData :: [Tag String] -> String -> Int -> ErrorM -> ErrorW String
getData tags key index err =
   do let fTags = dropWhile (~/= (TagText key)) tags
      case (length fTags) <= index of
         False -> do let (TagText toReturn) = fTags !! index
                     return toReturn
         True  -> throwError err
