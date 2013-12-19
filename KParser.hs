{-
   
   OLD: Abandonen parsec version of the html parser
        Doesn't really suit the HTML structure,
        instead I'm now using TagSoup, see the Scraper module

-}
module KParser (

     Company(..)
   , parsePage
   , run
   , doParse
   
   ) where

import Text.ParserCombinators.Parsec

data Company a = Value a

parsePage :: String -> IO String
parsePage input = return input

ktype :: Parser String
ktype = do t1 <- many1 alphaNum
           string "-"
           t2 <- many1 alphaNum
           spaces
           return (t1++"-"++t2)

header :: Parser (String,String,String)
header = do string "<DOCUMENT>"
            spaces
            string "<TYPE>"
            typ <- ktype
            string "<SEQUENCE>"
            sequence <- many1 digit
            spaces
            manyTill anyChar newline
            string "<DESCRIPTION>"
            typ2 <- ktype
            string "<TEXT>"
            return (typ,sequence,typ2)

-- statement of income
incomeStatement :: Parser (String,String,String)
incomeStatement = do many line
                     netSales <-string "<font style=\"font-family:inherit;font-size:10pt;font-weight:bold;\">Statements of Income Data:</font>"
                     grossProfit <- specificLine "Gross profit"
                     incomeLoss  <- specificLine "Income (loss) from operations"
                     return (netSales,grossProfit,incomeLoss)

line :: Parser String
line = do line <- many anyChar
          char '\n'
          return line

specificLine :: String -> Parser String
specificLine starter = manyTill anyChar newline


title :: Parser String
title = do spaces
           first <- letter
           varName <- many alphaNum
           spaces
           return (first:varName)

-- run title " woowow " 
run :: Show a => Parser a -> String -> IO String
run p input = case parse p "" input of
                   Left err ->
                      do putStrLn "parse error at "
                         return (show err)
                   Right x -> return (show x)

doParse :: String -> IO String
doParse str = run incomeStatement str
