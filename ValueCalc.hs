module ValueCalc (
   main
) where

import KParser

main :: IO ()
main = do putStrLn "Hello!"
          putStrLn "First commit!"
          putStrLn "What to come?!"
          morePrinting
          parsed <- doParse " wowowParseThis "
          putStrLn parsed

morePrinting :: IO ()
morePrinting = do putStrLn "Some testing"
                  putStrLn "Will this work?"

test :: [] Int
test = do aoa <- return 2
          let awd = 223
          return (35+aoa+awd)

test2 :: [] Int
test2 = (return 2) >>= 
           (\x->( (return 202) >>= (\y-> return x)))
