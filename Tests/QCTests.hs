module Main where

import Control.Monad
import Data.Maybe
import Test.QuickCheck
import Text.HTML.TagSoup
import Text.HTML.ValueCalc
import System.Exit (exitFailure)

-- running test suite
main :: IO ()
main =
  do quickCheck threeArrowTest
     r1  <- quickCheckResult          prop_getDiff
     r2  <- quickCheckResult          prop_isUnderValued
     r3  <- quickCheckResult          prop_whiteSpacesDropped
     r4  <- quickCheckResult          prop_dropEmpty
     r6  <- quickCheckResult          prop_isUnderValued
     r7  <- wrap genFromDolSign       prop_fromDolSign
     r8  <- wrap genFromMilDol        prop_fromMilDol
     r9  <- wrap genToMilSek          prop_toMilSek
     forM_ [r1, r2, r3, r4, r6, r7, r8, r9] qcToTest
  where wrap f p = quickCheckResult $ forAll f p

-- Property...
threeArrowTest :: Int -> Property
threeArrowTest i = (i /= 23) ==> i == i

{-
   For a Result, throw exitFailure if its not successful,
   otherwise return unit
-}
qcToTest :: Result -> IO ()
qcToTest (Success _ _ _) = return ()
qcToTest _               = exitFailure

-- Telling quickcheck how to generate a company.
instance Arbitrary Company where
  arbitrary = liftM3 (Company "Random Company") randInt randInt randInt
    where randInt = elements [-9999..9999] :: Gen Integer

{-
   Generators for specific kinds of strings needed
   for different tests. Some of these functions are
   only intended to work on a certain type of input
   strings which are gathered by scraping.

   "$ " is added before all instances of this string,
   but this is not stated here because of optimization

   okay filters the input space to discard meaningless tests
-}
genFromDolSign :: Gen String
genFromDolSign = suchThat (listOf $ elements "0123456789") okay
  where okay str = length str > 0

-- okay filters the input space to discard meaningless tests
genFromMilDol :: Gen String
genFromMilDol = suchThat (listOf $ elements "0123456789.,") okay
  where okay str = (beforeDot str) /= ""              &&
                   (length $ filter (=='.') str) <= 1 &&
                   str /= ""                          &&
                   head str /= '.'                    &&
                   head str /= ','

-- okay filters the input space to discard meaningless tests
genToMilSek :: Gen String
genToMilSek = suchThat (listOf $ elements "0123456789") okay
  where okay str = (str /= "") && noBeginningZero str

genFromCommanotation :: Gen String
genFromCommanotation = suchThat (listOf $ elements "0123456789,") okay
  where okay str = str /= ""       &&
                   head str /= ',' &&
                   elem ',' str    &&
                   last str /= ',' &&
                   noBeginningZero str

-- Testing the calculations module::getDiff
prop_getDiff :: Company -> Bool
prop_getDiff c = (ta-tl)-mc == getDiff c
  where ta = totalAssets c
        tl = totalLiabilities c
        mc = marketCap c

{- Testing the calculations module::isUnderValued
   See if the ta-tl is larger than the marketcap and compare it
   to the isUnderValued function from the calculations module
-}
prop_isUnderValued :: Company -> Bool
prop_isUnderValued c = ((ta - tl) > mc) == isUnderValued c
  where ta = totalAssets c
        tl = totalLiabilities c
        mc = marketCap c

{- in case it's TagText then we want to make sure a doesn't
   have whitespaces anymore in case of a Tag (that's not TagText)
   nothing should be changed in this case using ws to not use
-}
prop_whiteSpacesDropped :: String -> Bool
prop_whiteSpacesDropped "" = True
prop_whiteSpacesDropped x  =
  let ws = [' ','\t','\n','\v','\f','\r','\160']
  in dropWhitespace (TagText x) == TagText [c | c <- x, not $ elem c ws]

-- dropEmpty :: [Tag String] -> [Tag String]
-- i only want to
prop_dropEmpty :: [String] -> Bool
prop_dropEmpty []  = True
prop_dropEmpty inp =
  dropEmpty tagged == [x | x <- tagged, x /= (TagText "")]
  where tagged = map (\x -> TagText x) inp

-- when fromDolSign has been run, there should be no ',' in the result
-- if a lonely $ is sent in, then the result should be 0
prop_fromDolSign :: String -> Bool
prop_fromDolSign "$" = (show $ fromDolSign "$") == "0"
prop_fromDolSign str = (not $ elem ',' parsed) && (take 2 parsed) /= "$ "
  where parsed = show $ fromDolSign ("$ 0" ++ str)
                      -- optimizing, I want "$ 0" to be leading

beforeDot :: String -> String
beforeDot str = dropWhile (=='0') $ takeWhile (/='.') $ filter (/=',') str

-- this returns True when there are no 0's in the beginning of the input str
noBeginningZero :: String -> Bool
noBeginningZero str = (length $ takeWhile (=='0') str) == 0

-- no commas, no dot, right amount of 0's
prop_fromMilDol :: String -> Bool
prop_fromMilDol ""  = True
prop_fromMilDol "0" = True
prop_fromMilDol str =
  fromMilDol str == (read (beforeDot str)::Integer) * 1000000

-- just verify that the new one is 000000 larger
prop_toMilSek :: String -> Bool
prop_toMilSek str = str ++ "000000" == show (fromJust $ toMilSek str)
