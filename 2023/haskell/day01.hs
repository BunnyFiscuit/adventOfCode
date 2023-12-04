module Day1 where
import FileReader
import Data.List
import Data.Maybe

main :: IO ()
main = do
  contents <- readF' "1"
  --putStr $ "Part 1: "
  --putStrLn $ show $ sum $ map getDigits contents
  let x = map wordInt contents
  let y = map getDigits x
  putStrLn $ "Part 2: "
  putStrLn $ show y
  putStrLn $ show $ sum y

-- Find the first and last number
getDigits :: String -> Int
getDigits s = (getFirstDigit s * 10) + getFirstDigit (reverse s)

getFirstDigit :: String -> Int
getFirstDigit s = read x :: Int
  where x = take 1 $ dropWhile (isNothing . isInt) s

isInt :: Char -> Maybe Int
isInt c = case reads [c] :: [(Int, String)] of
  [(_, "")] -> Just (read [c] :: Int)
  _         -> Nothing

wordInt :: String -> String
wordInt []                       = []
wordInt ('o':'n':'e':xs)         = '1' : wordInt ('n':'e':xs)
wordInt ('t':'w':'o':xs)         = '2' : wordInt ('w':'o':xs)
wordInt ('t':'h':'r':'e':'e':xs) = '3' : wordInt ('h':'r':'e':'e':xs)
wordInt ('f':'o':'u':'r':xs)     = '4' : wordInt ('o':'u':'r':xs)
wordInt ('f':'i':'v':'e':xs)     = '5' : wordInt ('i':'v':'e':xs)
wordInt ('s':'i':'x':xs)         = '6' : wordInt ('i':'x':xs)
wordInt ('s':'e':'v':'e':'n':xs) = '7' : wordInt ('e':'v':'e':'n':xs)
wordInt ('e':'i':'g':'h':'t':xs) = '8' : wordInt ('i':'g':'h':'t':xs)
wordInt ('n':'i':'n':'e':xs)     = '9' : wordInt ('i':'n':'e':xs)
wordInt (x:xs)                   = case isInt x of
  Just _  -> x : wordInt xs
  Nothing -> wordInt xs