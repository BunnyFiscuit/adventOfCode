{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day11 where
import FileReader
import Data.List
import Data.Char (isSpace)
import qualified Data.Map as M

type Item      = Int
type Value     = Int
type Operation = (OpVal, Op, OpVal)
type Test      = Int
type Worry     = Int

data OpVal = Old | I Int deriving (Show, Read)
data Op    = Add | Mult deriving Show


type Monkeys = M.Map Int Monkey

data Monkey = Monkey {
  items  :: [Item],
  op     :: Operation,
  testv  :: Test,
  trueM  :: Int,
  falseM :: Int,
  inspected :: Int
}

instance Show Monkey where
  show :: Monkey -> String
  show Monkey { items, op, testv, trueM, falseM, inspected } 
    = "Monkey { items = " ++ show items 
    ++ ", op = " ++ show op
    ++ ", testv = " ++ show testv
    ++ ", trueM = " ++ show trueM
    ++ ", falseM = " ++ show falseM
    ++ ", inspected = " ++ show inspected ++ " }"

emptyMonkey = Monkey {
  items     = [],
  op        = (Old, Add, Old),
  testv     = 10,
  trueM     = 0,
  falseM    = 1,
  inspected = 0
}

buildMonkey :: [String] -> (Int, Monkey)
buildMonkey [m, starting, op, test, iftrue, iffalse, _] = buildMonkey [m, starting, op, test, iftrue, iffalse]
buildMonkey [m, starting, op, test, iftrue, iffalse] 
  = (m', Monkey { items = it, op = op', testv = tt, trueM = tm, falseM = fm, inspected = 0 })
  where
    m'  = extMonkeyInt m
    it  = extItems starting
    op' = extOperation op
    tt  = extTest test
    tm  = extMonkeyCond iftrue
    fm  = extMonkeyCond iffalse

splitMonkeys :: [String] -> [[String]]
splitMonkeys []     = []
splitMonkeys (x:xs) = ms : splitMonkeys xs'
  where ms  = x : takeWhile (not . isPrefixOf "Monkey") xs
        xs' = dropWhile (not . isPrefixOf "Monkey") xs

-- Monkey Id
extMonkeyInt :: String -> Int
extMonkeyInt s = read [x] :: Int
  where x = last (takeWhile (/= ':') s)

-- Monkey Items
extItems :: String -> [Item]
extItems s = spl s'
  where s' = drop 1 $ dropWhile (/=':') s
        spl [] = []
        spl xs = (read (takeWhile (/=',') xs) :: Int) : spl (drop 1 (dropWhile (/=',') xs))

-- Monkey Operation
extOperation :: String -> Operation
extOperation s = parseOp ops
  where ops = drop 2 $ dropWhile (/='=') s

parseOp :: String -> Operation
parseOp s = (fstVal, op', sndVal)
  where [v1, op, v2] = words s
        fstVal = toOpVal v1
        sndVal = toOpVal v2
        op'    = if op == "+" then Add else Mult

toOpVal :: String -> OpVal
toOpVal "old" = Old
toOpVal  x    = I (read x :: Int)

-- Monkey Test
extTest :: String -> Test
extTest s = val
  where val = read (last (words s)) :: Int

-- Monkey if true / false
extMonkeyCond :: String -> Int
extMonkeyCond s = read (last (words s)) :: Int

test :: Int -> Int -> Bool
test v t = v `mod` t == 0

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

run1 :: Monkeys -> IO ()
run1 mx = do
  print mx

doRound :: Monkeys -> Int -> IO (Monkeys, Int)
doRound mx index = undefined

runMonkey :: Monkeys -> Int -> IO Monkeys
runMonkey mx monkey = do
  let m = mx M.! monkey
  -- get item
  let (x:xs) = items m
  -- calculate worry level
  let val  = runOp (op m) x `div` 3
  let give = if test val (testv m) then trueM m else falseM m
  -- Monkey to give item
  let gm = mx M.! give
  return mx

runMonkey' :: Monkeys -> Int -> IO Monkeys
runMonkey' mx monkey = do
  let m = mx M.! monkey
  let ix = items m
  case ix of
    []      -> return mx
    (x:xs)  -> do
      -- calculate worry level
      let val  = runOp (op m) x `div` 3
      let give = if test val (testv m) then trueM m else falseM m
      -- Monkey to give item
      let gm = mx M.! give
      -- Give item to monkey
      -- Remove item from current monkey
      -- runMonkey' again
      return mx 

runOp :: Operation -> Int -> Int
runOp (Old, op, Old) v = doOp op v v
runOp (Old, op, I i) v = doOp op v i
runOp (I i, op, I j) v = doOp op i j

doOp :: Op -> Int -> Int -> Int
doOp Add  = (+)
doOp Mult = (*)

main :: IO ()
main = do
  contents <- readF' "11"
  let monkeys = map buildMonkey (splitMonkeys (map trim contents))
  let mx = M.fromList monkeys
  run1 mx