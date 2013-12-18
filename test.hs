import Network.HTTP
import Text.HTML.TagSoup
import Data.Char
import Data.List

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

getOpen :: IO String
getOpen = openURL "http://www.haskell.org/haskellwiki/Haskell"

getEdgar :: IO String
getEdgar = openURL "http://www.sec.gov/Archives/edgar/data/876883/000114420412014991/v303734_10k.htm"

mdca :: IO ()
mdca =
   do tags <- fmap parseTags $ getEdgar
      let count = fromSomething (head (sections (~== "<div>") tags))
      putStrLn $ "wooo" ++ show count ++ "..." 


fromSomething :: [Tag String] -> Int
fromSomething x =
   read (filter isDigit num) :: Int
   where num = ss !! (i-1)
         Just i = findIndex (=="Total Assets") ss
         ss = words s
         TagText s = (sections (~== "<td>") x) !! 1 !! 1

haskellHitCount :: IO ()
haskellHitCount = do
   tags <- fmap parseTags $ getOpen
   let count = fromFooter (head (sections (~== "<ul id=\"f-list\">") tags))
   putStrLn $ "hit" ++ show count ++ "..."

fromFooter :: [Tag String] -> Int 
fromFooter x =
   read (filter isDigit num) :: Int
   where num = ss !! (i-1)
         Just i = findIndex (== "times.") ss
         ss = words s
         TagText s = (sections (~== "<li>") x) !! 1 !! 1
