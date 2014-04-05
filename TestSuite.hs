import Test.QuickCheck
import Text.HTML.TagSoup
import Scraping      (Company (..), dropWhitespace, dropEmpty)
import NasdaqScraper (fromMilDol, fromDolSign)
import OMXScraper    (toMilSek, toBilSek, fromCommanotation)
import Calculations  (getDiff, isUnderValued)

-- running test suite, TODO: better way of defining a quickCheck test suite?
main :: IO ()
main = do quickCheck prop_unit
          quickCheck prop_whiteSpacesDropped
          quickCheck prop_dropEmpty
          quickCheck prop_getDiff
          quickCheck prop_isUnderValued
          quickCheck $ forAll genFromDolSign       prop_fromDolSign
          quickCheck $ forAll genFromMilDol        prop_fromMilDol
          quickCheck $ forAll genToMilSek          prop_toMilSek
          quickCheck $ forAll genToBilSek          prop_toBilSek
          quickCheck $ forAll genFromCommanotation prop_fromCommanotation

-- Telling quickcheck how to generate a company.
instance Arbitrary Company where
   arbitrary = do int1 <- randInt
                  int2 <- randInt
                  int3 <- randInt
                  return $ Company "Random Company" int1 int2 int3
               where randInt = elements [-9999..9999] :: Gen Integer

{-
   Generators for specific kinds of strings needed
   for different tests. Some of these functions are
   only intended to work on a certain type of input
   strings which are gathered by scraping.

   TODO: Generate more useful strings
-}
genFromDolSign :: Gen String
genFromDolSign = do a <- elements ["$ 20","$ 50"]
                    return a

genFromMilDol :: Gen String
genFromMilDol = suchThat (listOf (elements "0123456789.,")) okayString
   where okayString str = (beforeDot str) /= [] && (length $ filter (=='.') str) <= 1 && 
                          str /= "" && head str /= '.' && head str /= ','

genToMilSek :: Gen String
genToMilSek = do a <- elements ["1","2"]
                 return a

genToBilSek :: Gen String
genToBilSek = do a <- elements ["1","3"]
                 return a

genFromCommanotation :: Gen String
genFromCommanotation = do a <- elements ["8","9"]
                          return a

{- @how: quickCheck prop_unit
   Just a unit function doing nothing.
   TODO: What can I do to test this?
-}
prop_unit :: [Company] -> Bool
prop_unit ls = ls == ls

{- in case it's TagText then we want to make sure a doesn't have whitespaces anymore
   in case of a Tag (that's not TagText) nothing should be changed in this case
   using ws to not use 
-}
prop_whiteSpacesDropped :: String -> Bool
prop_whiteSpacesDropped "" = True
prop_whiteSpacesDropped x  =
   let ws = [' ','\t','\n','\v','\f','\r','\160']
   in dropWhitespace (TagText x) == TagText [c|c<-x,not $ elem c ws]

-- dropEmpty :: [Tag String] -> [Tag String]
-- i only want to 
prop_dropEmpty :: [String] -> Bool
prop_dropEmpty []  = True
prop_dropEmpty inp =
   dropEmpty tagged == [x|x<-tagged,x/=(TagText "")]
      where tagged = map (\x->TagText x) inp

-- when fromDolSign has been run, there should be no ',' in the result
-- if a lonely $ is sent in, then the result should be 0
prop_fromDolSign :: String -> Bool
prop_fromDolSign "$" = (show $ fromDolSign "$") == "0"
prop_fromDolSign str = (not $ elem ',' parsed) && (take 2 parsed) /= "$ "
   where parsed      = show $ fromDolSign str

beforeDot :: String -> String
beforeDot str = dropWhile (=='0') $ takeWhile (/='.') $ filter (/=',') str

-- no commas, no dot, right amount of 0's
prop_fromMilDol :: String -> Bool
prop_fromMilDol ""  = True
prop_fromMilDol "0" = True
prop_fromMilDol str = fromMilDol str == (read (beforeDot str) :: Integer) * 1000000

-- TODO: Same TODO as above
prop_toMilSek :: String -> Bool
prop_toMilSek str = True

-- TODO: Same TODO as above
prop_toBilSek :: String -> Bool
prop_toBilSek str = True

-- TODO: Same TODO as above
prop_fromCommanotation :: String -> Bool
prop_fromCommanotation str = True

-- Calculations
-- the valuation calculations gives back positive or 
-- negative correctly (right side of 0)
prop_getDiff :: Integer -> Integer -> Integer -> Bool
prop_getDiff as lia mc
   | as-lia >  mc = getDiff as lia mc >  0 && isUnderValued as lia mc       --under
   | as-lia == mc = getDiff as lia mc == 0 && not (isUnderValued as lia mc) --even
   | as-lia <  mc = getDiff as lia mc <  0 && not (isUnderValued as lia mc) --over

-- just test if undervalued and overvalued works
prop_isUnderValued :: Integer -> Integer -> Integer -> Bool
prop_isUnderValued as lia mc
   | as-lia > mc = isUnderValued as lia mc == True
   | otherwise   = isUnderValued as lia mc == False
