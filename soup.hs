module TagSoup.Sample where

import Text.HTML.TagSoup

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Cmd
import System.Directory
import System.Exit
import System.IO

openItem :: String -> IO String
openItem url | not $ "http://" `isPrefixOf` url = readFile url
openItem url = bracket
    (openTempFile "." "tagsoup.tmp")
    (\(file,hndl) -> removeFile file)
    $ \(file,hndl) -> do
        hClose hndl
        putStrLn $ "Downloading: " ++ url
        res <- system $ "curl " ++ url ++ " -o " ++ file
        when (res /= ExitSuccess) $ error $ "Failed to download using curl: " ++ url
        src <- readFile file
        length src `seq` return src

grab :: String -> IO ()
grab x = openItem x >>= putStr

fromFooter x = read (filter isDigit num) :: Int
   where num = ss !! (i-1)
         Just i = findIndex (== "times.") ss
         ss = words s
         TagText s = sections (~== "<p>") x !! 1 !! 1

spjPapers :: IO ()
spjPapers = do
   tags <- fmap parseTags $ openItem "http://research.microsoft.com/en-us/people/simonpj/"
   let links = map f $ sections (~== "<A>") $ takeWhile (~/= "<A name=haskell>") $ drop 5 $ dropWhile (~/= "<A name=current>") tags
   putStr $ unlines links
   where
      f :: [Tag] -> String
      f = dequote . unwords . words . fromTagText . head . filter isTagText

      dequote ('\"':xs) | last xs == '\"' = init xs
      dequote x = x

parse :: String -> IO ()
parse x = openItem x >>= putStr . show2 . parseTags
    where
        show2 [] = "[]"
        show2 xs = "[" ++ concat (intersperseNotBroken "\n," $ map show xs) ++ "\n]\n"

-- the standard intersperse has a strictness bug which sucks!
intersperseNotBroken :: a -> [a] -> [a]
intersperseNotBroken _ [] = []
intersperseNotBroken sep (x:xs) = x : is xs
    where
        is [] = []
        is (y:ys) = sep : y : is ys
