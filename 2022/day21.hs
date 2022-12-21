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
  let rootValue = doOp op (getValue m1 screams) (getValue m2 screams)
  putStrLn $ "part 1: " ++ show rootValue

data Op = Add | Sub | Mult | Div deriving (Show, Eq, Read)
type Monkey = String

type Screams = M.Map Monkey (Either Expr Int)
type Expr = (String, Op, String)

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
  Just (Left  e)   -> e
  Just (Right _)   -> error "Root lookup should not have value"

getValue :: Monkey -> Screams -> Int
getValue monkey xs = case M.lookup monkey xs of
  Nothing -> error "No such monkey"
  Just (Left (m1, op, m2))  -> doOp op (getValue m1 xs) (getValue m2 xs)
  Just (Right v) -> v

