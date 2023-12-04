{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day11 where
import FileReader
import Data.List
import Data.Char (isSpace)
import qualified Data.Map as M
import Control.Monad

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

-- Run
inspCompare :: Monkey -> Monkey -> Ordering
inspCompare m1 m2
  | m1i > m2i = LT
  | otherwise = GT
    where m1i = inspected m1
          m2i = inspected m2

monkeyBusiness :: Monkeys -> IO Int
monkeyBusiness mx = do
  -- descending inspected value
  let sorted = sortBy inspCompare (map snd (M.toList mx))
  let v1 = inspected $ head sorted
  let v2 = inspected $ sorted !! 1
  return (v1 * v2)

doRounds :: Monkeys -> Int -> IO Monkeys
doRounds mx 0  = return mx
doRounds mx n  = doRound mx 0 >>= \mx' -> doRounds mx' (n-1)

doRounds2 :: Monkeys -> Int -> IO Monkeys
doRounds2 mx 0  = return mx
doRounds2 mx n  = doRound2 mx 0 >>= \mx' -> doRounds2 mx' (n-1)

doRound :: Monkeys -> Int -> IO Monkeys
doRound mx index
  | index < M.size mx = runMonkey mx index >>= \mx' -> doRound mx' (index+1)
  | otherwise = return mx

runOp :: Operation -> Int -> Int
runOp (Old, op, Old) v = doOp op v v
runOp (Old, op, I i) v = doOp op v i
runOp (I i, op, I j) v = doOp op i j

doOp :: Op -> Int -> Int -> Int
doOp Add  = (+)
doOp Mult = (*)

run1 :: Monkeys -> IO ()
run1 mx = do
  mx' <- doRounds mx 20
  printMonkeys mx'
  mb <- monkeyBusiness mx'
  print mb

runMonkey :: Monkeys -> Int -> IO Monkeys
runMonkey mx monkey = do
  let m = mx M.! monkey
  let ix = items m
  case ix of
    []      -> return mx
    (x:xs)  -> do
      -- calculate worry level
      let val  = runOp (op m) x `div` 3
      -- Get the monkey number to give val to
      let give = if test val (testv m) then trueM m else falseM m
      -- Get the 'give' monkey
      let gm  = mx M.! give
      -- Give item to monkey
      let gm' = gm { items = items gm ++ [val]}
      -- Remove item from current monkey
      let m'  = m { items = xs, inspected = inspected m + 1 }
      -- Update mx with new give monkey
      let mx' = M.update (\x -> Just gm') give mx
      -- runMonkey' again
      runMonkey (M.update (\x -> Just m') monkey mx') monkey

runMonkey2 :: Monkeys -> Int -> Int -> IO Monkeys
runMonkey2 mx monkey superm = do
  let m = mx M.! monkey
  let ix = items m
  case ix of
    []      -> return mx
    (x:xs)  -> do
      -- calculate worry level
      let val  = runOp (op m) x `mod` superm
      -- Get the monkey number to give val to
      let give = if test val (testv m) then trueM m else falseM m
      -- Get the 'give' monkey
      let gm  = mx M.! give
      -- Give item to monkey
      let gm' = gm { items = items gm ++ [val]}
      -- Remove item from current monkey
      let m'  = m { items = xs, inspected = inspected m + 1 }
      -- Update mx with new give monkey
      let mx' = M.update (\x -> Just gm') give mx
      -- runMonkey' again
      runMonkey2 (M.update (\x -> Just m') monkey mx') monkey superm

run2 :: Monkeys -> IO ()
run2 mx = do
  mx' <- doRounds2 mx 10000
  printMonkeys mx'
  mb <- monkeyBusiness mx'
  print mb

doRound2 :: Monkeys -> Int -> IO Monkeys
doRound2 mx index
  | index < M.size mx = supermod mx >>= \sm -> runMonkey2 mx index sm >>= \mx' -> doRound2 mx' (index+1)
  | otherwise = return mx

supermod :: Monkeys -> IO Int
supermod mx = return $ product [ testv m | m <- map snd (M.toList mx)]


main :: IO ()
main = do
  contents <- readF' "11"
  let monkeys = map buildMonkey (splitMonkeys (map trim contents))
  let mx = M.fromList monkeys
  run1 mx
  putStrLn "------"
  run2 mx

printMonkeys :: Monkeys -> IO ()
printMonkeys mx = do
  let mxs = M.toList mx
  mapM_ (putStrLn . show) mxs