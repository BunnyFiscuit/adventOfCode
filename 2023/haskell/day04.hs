module Day4 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "test"
  let cards = readCards contents
  putStrLn $ show cards ++ "\n"
  --putStrLn $ show $ part1 cards
  part2 cards cards 0
  return ()

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
readCard s = Card n ws ys -- Card winning yours
  where ws = map readInt (takeWhile (/="|") ds)
        ys   = map readInt (drop 1 (dropWhile (/="|") ds))
        ds   = words $ drop 1 $ dropWhile (/=':') s
        n    = readInt (take 2 (dropWhile (/=' ') s))

part1 :: [Card] -> Int
part1 = foldr ((+) . cardPoints) 0

cardPoints :: Card -> Int
cardPoints (Card n ws ys) = if n == 0 then 0 else 2 ^ (n-1)
  where n = sum $ [ 1 | x <- ys, x `elem` ws]

matches :: Card -> Int
matches (Card n ws ys) = sum $ [ 1 | x <- ys, x `elem` ws]


part2 :: [Card] -> [Card] -> Int -> IO Int
part2 _ [] n = do
  print n
  return n
part2 [] _ n = do
  print n
  return n
part2 (c:cs) (d:ds) n = do
  let xs = processCard cs c
  let cm = sortCards $ xs ++ cs
  putStrLn $ show (n+1) ++ "\n" ++ show cm ++ "\n"
  part2 cs cm (n+1)

processCard :: [Card] -> Card -> [Card]
processCard cs x = filter (\c -> nr c /= nr x) (take p cs)
  where p = matches x

sortCards :: [Card] -> [Card]
sortCards = sortBy (\x y -> compare (nr x) (nr y))