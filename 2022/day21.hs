{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day21 where
import FileReader
import Data.List
import Data.Maybe
import Text.Read

main :: IO ()
main = do
  contents <- readF' "21"
  let screams = map readLine contents
  let root@(MOp m (S va) op (S vb)) = findRoot screams
  print va
  print vb
  let rootValue = doOp op (getValue va 0 screams) (getValue vb 0 screams)
  putStrLn $ "part 1: " ++ show rootValue

data Op = Add | Sub | Mult | Div deriving (Show, Eq, Read)
data Value = S String | I Int deriving (Show, Eq, Read)
data Expr = MOp Monkey Value Op Value | MVal String Value  deriving (Show, Eq, Read)

type Monkey = String

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

readValue :: String -> Value
readValue str = case readMaybe str :: Maybe Int of
  (Just v) -> I v
  Nothing  -> S str

readLine :: String -> Expr
readLine str = case length ws of
  2 -> MVal monkey (readValue (ws !! 1))
  4 -> MOp monkey (readValue (ws !! 1)) (readOp (ws !! 2)) (readValue (ws !! 3))
  _ -> error "Should not happen"
  where 
    ws = words str
    monkey = takeWhile (/=':') (head ws)
    firstVal = readMaybe (ws !! 1) :: Maybe Int
    
findRoot :: [Expr] -> Expr
findRoot [] = error "No root"
findRoot (x:xs)
  | getMonkey x == "root" = x
  | otherwise = findRoot xs

getValue :: Monkey -> Int -> [Expr] -> Int
getValue monkey n xs
  | monkey == m = case current of
  MOp s (S va) op (S vb) -> doOp op (getValue va 0 xs) (getValue vb 0 xs)
  MVal s (I va) -> va

  | otherwise = getValue monkey (n+1) xs
  where 
    current = head $ take 1 $ drop n xs
    m = getMonkey current

getMonkey :: Expr -> Monkey
getMonkey (MVal m _) = m
getMonkey (MOp m _ _ _) = m
