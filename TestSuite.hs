import Test.QuickCheck
import Scraping (Company (..))

{--

I want to run tests around the Company data type...

data Company = Company {
  name             :: String,
  totalAssets      :: Integer,
  totalLiabilities :: Integer,
  marketCap        :: Integer
  
} deriving (Show,Read)

--}

instance Arbitrary Company where
   arbitrary = do int1 <- randInt
                  int2 <- randInt
                  int3 <- randInt
                  return $ Company "Random Company" int1 int2 int3
               where randInt = elements [-9999..9999] :: Gen Integer

-- @how: quickCheck prop_unit
-- Just a unit function doing nothing..
prop_unit :: [Company] -> Bool
prop_unit ls = ls == ls
