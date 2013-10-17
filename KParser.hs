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
doParse str = run header str
