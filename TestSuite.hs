import Test.QuickCheck
import Text.HTML.TagSoup
import Scraping      (Company (..), dropWhitespace, dropEmpty)
import NasdaqScraper (fromMilDol, fromDolSign)
import OMXScraper    (toMilSek, toBilSek, fromCommanotation)
import Calculations  (getDiff, isUnderValued)

-- running test suite, TODO: better way of defining a quickCheck test suite?
main :: IO ()
main = do
   quickCheck prop_unit
   quickCheck prop_whiteSpacesDropped
   quickCheck prop_dropEmpty
   quickCheck prop_getDiff
   quickCheck prop_isUnderValued
   quickCheck prop_fromDolSign
   quickCheck prop_fromMilDol
   quickCheck prop_toMilSek
   quickCheck prop_toBilSek
   quickCheck prop_fromCommanotation 

-- Telling quickcheck how to generate a company.
instance Arbitrary Company where
   arbitrary = do int1 <- randInt
                  int2 <- randInt
                  int3 <- randInt
                  return $ Company "Random Company" int1 int2 int3
               where randInt = elements [-9999..9999] :: Gen Integer

-- @how: quickCheck prop_unit
-- Just a unit function doing nothing.
-- TODO: What can I do to test this?
prop_unit :: [Company] -> Bool
prop_unit ls = ls == ls

-- in case it's TagText then we want to make sure a doesn't have whitespaces anymore
-- in case of a Tag (that's not TagText) nothing should be changed in this case
-- using ws to not use 
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
      where tagged = (map (\x->TagText x) inp)

-- TODO: NasdaqScraper
-- TODO: This needs to generate the appropriate kind of strings
prop_fromDolSign :: String -> Bool
prop_fromDolSign str = True

-- TODO: Same TODO as above
prop_fromMilDol :: String -> Bool
prop_fromMilDol str = True

-- TODO OMXScraper
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
-- the valuation calculations gives back positive or negative correctly (right side of 0)
prop_getDiff :: Integer -> Integer -> Integer -> Bool
prop_getDiff asst lia mc | asst-lia >  mc = getDiff asst lia mc >  0 -- undervalued
                           && isUnderValued asst lia mc
                         | asst-lia == mc = getDiff asst lia mc == 0 -- perfect
                           && not (isUnderValued asst lia mc)
                         | asst-lia <  mc = getDiff asst lia mc <  0 -- overvalued
                           && not (isUnderValued asst lia mc)

-- just test if undervalued and overvalued works
prop_isUnderValued :: Integer -> Integer -> Integer -> Bool
prop_isUnderValued asst lia mc | asst-lia > mc = isUnderValued asst lia mc == True
                               | otherwise     = isUnderValued asst lia mc == False
