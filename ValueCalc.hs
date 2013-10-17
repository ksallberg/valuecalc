module ValueCalc (
   main
) where

import KParser

main :: IO ()
main = do putStrLn "Hello!"
          file <- readFile "DPS-10K-12.31.12.html"
          putStrLn "First commit!"
          putStrLn "What to come?!"
          morePrinting
          parsed <- doParse (take 1000 file)
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
