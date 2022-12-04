module Day12 where
import System.IO
import FileReader
import Data.Char
import Data.List
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = do
  contents <- readF 12
  -- putStrLn $ show contents
  let ps = parseStr contents
  putStrLn $ show ps

type EdgeM = Map Node [Node]

type EState a = State EdgeM a

-- type EState a = State MRecord a

data Node  = Start | End | Bg String | Sm String deriving (Show, Eq)
-- data Node  = Start | End | B String | S String deriving (Show, Eq, Ord)
type Edge  = (Node, Node) 
type MultiEdge = (Node, [Node])
type Edges = [Edge]
type Path  = [Node]
type Paths = [Path]

parseStr :: [String] -> [Edge]
parseStr [] = []
parseStr (x:xs) = (str2Node start, str2Node end) : parseStr xs
  where start = takeWhile (/='-') x
        end   = drop 1 (dropWhile(/='-') x)

str2Node :: String -> Node
str2Node "start" = Start
str2Node "end"   = End
str2Node x       = toCave x

toCave :: String -> Node
toCave x = if isBigCave x then Bg x else Sm x

correctEdges :: Edges -> Edges
correctEdges [] = []
correctEdges ((e,Start):es) = (Start,e) : correctEdges es
correctEdges ((End,e)  :es) = (e,End) : correctEdges es
correctEdges (e:es)             = e : correctEdges es 

startEdges :: Edges -> Edges
startEdges es = filter isStartEdge es

isStartEdge :: Edge -> Bool
isStartEdge (Start,_) = True
isStartEdge _         = False

groupEdges :: Edges -> [Edges]
groupEdges = groupBy (\x y -> fst x == fst y)

makeMultiEdge :: [Edges] -> [MultiEdge]
makeMultiEdge = undefined

isBigCave :: String -> Bool
isBigCave xs = and (map isUpper xs)

ex = [
  (Start,B "A"),(Start,S "b"),
  (B "A",S "c"),(B "A",S "b"),(B "A",End),
  (S "b",S "d"),(S "b",End)
  ]

mapEm :: Edges -> EState EdgeM
mapEm [] = get
mapEm ((f, t):xs) = do
  m <- get
  case Map.lookup f m of
    Just v -> do
      put (Map.insert f (v ++ [t]) m)
      mapEm xs
    Nothing -> do
      put (Map.insert f [t] m)
      mapEm xs

runIt :: Node -> EState Paths
runIt Start = do
  m <- get
  case Map.lookup Start m of
    Just t -> do
      put (Map.delete Start m)
      ps <- mapM runIt t
      let ms = map (Start:) (concat ps)
      return $ ms
    Nothing -> return []

runIt End = return [[End]]
runIt c = do
  m <- get
  case Map.lookup c m of
    Just t -> do
      put (Map.delete c m)
      undefined
    Nothing -> return [[c]]


makePath :: (Node, [Node]) -> Paths
makePath (n, []) = []
makePath (n, x:xs) = [n,x] : makePath (n, xs)

isCorrectPath :: Path -> Bool
isCorrectPath (Start:[]) = False
isCorrectPath (Start: xs) = last xs == End
isCorrectPath          _  = False