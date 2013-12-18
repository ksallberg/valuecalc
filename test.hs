import Network.HTTP
import Text.HTML.TagSoup
import Data.Char
import Data.List

main :: IO ()
main = do
    http <- simpleHTTP (getRequest "http://www.cbssports.com/nba/scoreboard/20130310") >>= getResponseBody
    let tags = dropWhile (~/= TagOpen "div" []) (parseTags http)
    done tags
      where done xs = case xs of
                        [] -> putStrLn $ "\n"
                        _ -> do
                           putStrLn $ show $ head xs
                           --done (tail xs)

openURL x = getResponseBody =<< simpleHTTP (getRequest x)

getOpen :: IO String
getOpen = openURL "http://www.haskell.org/haskellwiki/Haskell"

getEdgar :: IO String
getEdgar = openURL "http://www.sec.gov/Archives/edgar/data/876883/000114420412014991/v303734_10k.htm"

mdcTag :: String
mdcTag = "<div style=\"width: 708px; border-top: 1pt white solid; border-bottom: 1pt white solid; border-right: 1pt white solid; border-left: 1pt white solid; padding-top: 9pt; padding-bottom: 9pt; padding-right: 3pt; padding-left:6pt; margin-top:6pt; margin-right: 0pt; margin-left:0pt; margin-bottom:6pt\" align=\"CENTER\">"

finder :: String
--finder = "<tr style="background-color: #CCFFCC\">"
finder = "<div>"

mdca :: IO ()
mdca =
   do tags <- fmap parseTags $ getEdgar
      let ccc = (sections (~== finder) tags)
      let (TagText count) = (sections (~== finder) tags) !! 1 !! 1
      let www = findIndex (=="Total Assets") (words count)
      let i = 12
      case www of
         Nothing -> putStrLn "heey"
         (Just x) -> putStrLn ("hoo" ++ show x)
      putStrLn (show www) 
      putStrLn $ "heeey" ++ show i
      putStrLn $ "wooo" ++ show count ++ "..." 

fromSomething :: [Tag String] -> String
fromSomething x =
   read (num) :: String
   where num = ss !! (i-1)
         Just i = findIndex (=="Total Assets") ss
         ss = words s
         TagText s = (sections (~== finder) x) !! 1 !! 1

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
