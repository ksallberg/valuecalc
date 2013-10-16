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
doParse str = run title str
