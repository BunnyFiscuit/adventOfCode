import System.IO
import Data.List as DL
import Data.Char
import Data.Maybe
import Debug.Trace

main :: IO ()
main = do
  content <- readFile "inputs/4.txt"
  let ls = lines content
  let firstRow = DL.take 1 ls
  let ns = numbers (head firstRow)
  let boxes = str2Boxes (DL.drop 2 ls)
  -- ans <- runBingo ns boxes
  -- putStrLn $ show ans
  ans2 <- runSquid ns boxes
  putStrLn $ show ans2

type Cell = Maybe Int
type Row = [Cell]
type Box = [Row]
type Boxes = [Box]

stringBox :: Box -> String
stringBox [] = ""
stringBox (b:bs) = concat [if isNothing x then " .  " else " " ++ show (fromJust x) ++ " " | x <- b]
  ++ "\n" ++ stringBox bs

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- input : ["1 2", "3 4", "5 6", "7 8"] 
-- output: [["1 2", "3 4"], ["5 6", "7 8"]]
getBoxes :: [String] -> [[String]]
getBoxes []  = []
getBoxes xs  = take 5 xs : getBoxes drpd
  where drpd = drop 6 xs

-- input : "1,2,3,4"
-- output: [1,2,3,4]
numbers :: String -> [Int]
numbers [] = []
numbers xs = r : numbers (DL.drop 1 d)
  where r = read g :: Int
        g = DL.takeWhile (/=',') xs
        d = DL.dropWhile (/=',') xs

-- input : "10 20 30"
-- output: [Just 10, Just 20, Just 30]
fr :: String -> Row
fr [] = []
fr xs = Just r : fr d
  where r = read g :: Int
        g = trim (take 2 xs)
        d = drop 3 xs

-- input : ["1 2", "3 4", "5 6", "7 8"]
-- output: [[[Just 1, Just 2],[Just 3, Just 4]],[[Just 5, Just 6], [Just 7, Just 8]]]
str2Boxes :: [String] -> Boxes
str2Boxes xs = map (\x -> map fr x) bs
  where bs = getBoxes xs

markBox :: Int -> Box -> Box
markBox _ [] = []
markBox n (row:rows) = [
  if (isNothing x) then Nothing else
  if (fromJust x == n) then Nothing else x | x <- row
  ] : markBox n rows 

markBoxes :: Int -> Boxes -> Boxes
markBoxes n bs = map (markBox n) bs

isBingoBox :: Box -> Bool
isBingoBox bs = checkHorizontal || checkVertical
  where bs' = map (map isNothing) bs
        ts  = transpose bs'
        checkHorizontal = any (==True) (map and bs')
        checkVertical = any (==True) (map and ts)

checkBingo :: Boxes -> Box
checkBingo [] = []
checkBingo (b:bs) = if isBingoBox b then b else checkBingo bs

removeBingoBoxes :: Boxes -> Boxes
removeBingoBoxes [] = []
removeBingoBoxes (b:bs) 
  | isBingoBox b = removeBingoBoxes bs
  | otherwise    = b : removeBingoBoxes bs

sumAndMultiply :: Int -> Box -> Int
sumAndMultiply n bs = sum (map fromJust bs') * n
  where bs' = filter (isJust) (concat bs)

runBingo :: [Int] -> Boxes -> IO Int
runBingo [] _ = return (-1)
runBingo (x:xs) bs = do
  let bs' = markBoxes x bs
  case checkBingo bs' of
    []  -> do
      runBingo xs bs'
    box -> do
      putStrLn $ stringBox box
      return (sumAndMultiply x box)

runSquid :: [Int] -> Boxes -> IO Int
runSquid [] _  = return (-1)
runSquid (x:xs) bs = do
  let bs' = markBoxes x bs
  let fs = removeBingoBoxes bs'
  putStrLn $ show (length fs)
  case length fs of
    1 -> do
      putStrLn $ show (x:xs)
      putStrLn $ stringBox (head fs)
      runBingo xs fs
    _ -> runSquid xs bs'
  