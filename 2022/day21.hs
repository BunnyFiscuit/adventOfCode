{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day21 where
import FileReader
import Data.List
import Data.Maybe
import Text.Read
import qualified Data.Map as M

main :: IO ()
main = do
  contents <- readF' "21"
  let screams = M.fromList (map readLine contents)
  let root@(m1, op, m2) = findRoot screams
  print m1
  print m2
  --let rootValue = doOp op (getValue m1 screams) (getValue m2 screams)
  --putStrLn $ "part 1: " ++ show rootValue
  putStrLn "---"
  let m1Humn = hasHumn m1 screams
  let val = if m1Humn then getValue m2 screams else getValue m1 screams
  let l   = if m1Humn then m2 else m1
  part2 l val screams

data Op = Add | Sub | Mult | Div deriving (Show, Eq, Read)
type Monkey = String

type Screams = M.Map Monkey (Either Expr Int)
type Expr = (String, Op, String)
type Root = (String, Op, String)

readOp :: String -> Op
readOp "+" = Add
readOp "-" = Sub
readOp "*" = Mult
readOp "/" = Div

doOp :: Op -> Int -> Int -> Int
doOp Add  a b = a + b
doOp Sub  a b = a - b
doOp Mult a b = a * b
doOp Div  a b = div a b

opOp :: Op -> Op
opOp Add  = Sub
opOp Sub  = Add
opOp Mult = Div
opOp Div  = Mult

readLine :: String -> (Monkey, Either Expr Int)
readLine str = case length ws of
  2 -> (monkey, Right (read (ws !! 1) :: Int))
  4 -> (monkey, Left (ws !! 1, readOp (ws !! 2), ws !! 3))
  where
    ws = words str
    monkey = takeWhile (/=':') (head ws)

findRoot :: Screams -> Expr
findRoot xs = case M.lookup "root" xs of
  Nothing  -> error "No root"
  Just (Right _)   -> error "Root lookup should not have value"
  Just (Left  e)   -> e

getValue :: Monkey -> Screams -> Int
getValue monkey xs = case M.lookup monkey xs of
  Nothing -> error "No such monkey"
  Just (Left (m1, op, m2))  -> doOp op (getValue m1 xs) (getValue m2 xs)
  Just (Right v) -> v

hasHumn :: Monkey -> Screams -> Bool
hasHumn "humn" _  = True
hasHumn monkey xs = case M.lookup monkey xs of
  Nothing -> False
  Just (Left ("humn", _, _)) -> True
  Just (Left (_, _, "humn")) -> True
  Just (Left (m1, _, m2)) -> hasHumn m1 xs || hasHumn m2 xs
  Just (Right _)           -> False

findMonkeyOp :: Monkey -> Monkey -> Screams -> Maybe Expr
findMonkeyOp looking at xs = case M.lookup at xs of
  Nothing -> Nothing
  Just (Left (m1, op, m2)) -> case m1 == looking of
    True  -> Just (looking, op, m2)
    False -> case m2 == looking of
      True  -> Just (m1, op, looking)
      False -> case findMonkeyOp looking m1 xs of
        Just x1 -> Just x1
        Nothing -> case findMonkeyOp looking m2 xs of
          Just x2 -> Just x2
          Nothing -> Nothing 
  Just (Right _)           -> Nothing

findParent :: Monkey -> Monkey -> Screams -> Maybe Monkey
findParent "root" _  _  = Nothing
findParent monkey at xs = case M.lookup at xs of
  Nothing -> Nothing
  Just (Left (m1, op, m2)) -> case (m1 == monkey) || (m2 == monkey) of
    True  -> Just at
    False -> case findParent monkey m1 xs of
      Nothing -> findParent monkey m2 xs
      Just s -> Just m1
  Just (Right v)           -> Just at

data MExpr = MVar String | MExpr MExpr Op MExpr | MInt Int deriving (Read, Show)

part2 :: Monkey -> Int -> Screams -> IO ()
part2 monkey val screams = do
  undefined

