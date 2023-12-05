module Day4 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "test"
  let cards = readCards contents
  --putStrLn $ show cards ++ "\n"
  putStrLn $ show $ part1 cards
  let p2 = part2 cards cards 0
  putStrLn $ show p2

data Card = Card Int [Int] [Int] deriving (Show)

winning :: Card -> [Int]
winning (Card _ ws _) = ws

yours :: Card -> [Int]
yours (Card _ _ ys) = ys

nr :: Card -> Int
nr (Card n _ _ ) = n

cstr = "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
card = Card 1 [41,48,83,86,17] [83,86,6,31,17,9,48,53]

readInt x = read x :: Int

readCards :: [String] -> [Card]
readCards = map readCard

readCard :: String -> Card
readCard s = Card n ws ys
  where ws   = map readInt (takeWhile (/="|") ds)
        ys   = map readInt (drop 1 (dropWhile (/="|") ds))
        ds   = words $ drop 1 $ dropWhile (/=':') s
        n    = readInt (takeWhile (/=':') (dropWhile (/=' ') s))

part1 :: [Card] -> Int
part1 = foldr ((+) . cardPoints) 0

cardPoints :: Card -> Int
cardPoints (Card n ws ys) = if n == 0 then 0 else 2 ^ (n-1)
  where n = sum $ [ 1 | x <- ys, x `elem` ws]

matches :: Card -> Int
matches (Card n ws ys) = sum $ [ 1 | x <- ys, x `elem` ws]


part2 :: [Card] -> [Card] -> Int -> Int
part2 _ [] n = n
part2 cs (d:ds) n = part2 cs cm (n+1)
  where xs = processCard cs d
        cm = sortCards $ xs ++ ds

processCard :: [Card] -> Card -> [Card]
processCard cs x = take p (drop (nr x) cs)
  where p = matches x

sortCards :: [Card] -> [Card]
sortCards = sortBy (\x y -> compare (nr x) (nr y))

load :: IO [Card]
load = do
  contents <- readF' "test"
  return $ readCards contents