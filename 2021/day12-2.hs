module Day12 where
import System.IO
import FileReader
import Data.Char
import Data.List
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Node  = Start | End | B String | S String deriving (Show, Eq, Ord)
type Edge  = (Node, Node) 
type MultiEdge = (Node, [Node])
type Edges = [Edge]
type Path  = [Node]

main :: IO ()
main = do
  contents <- readF 12
  -- putStrLn $ show contents
  let ps = parseStr contents
  putStrLn $ show ps
  -- mapM_ (putStrLn . show) ps

parseStr :: [String] -> [(Node, Node)]
parseStr [] = []
parseStr (x:xs) = (str2Node start, str2Node end) : parseStr xs
  where start = takeWhile (/='-') x
        end   = drop 1 (dropWhile(/='-') x)

str2Node :: String -> Node
str2Node "start" = Start
str2Node "end"   = End
str2Node x       = toCave x

toCave :: String -> Node
toCave x = if isBigCave x then B x else S x

correctEdges :: Edges -> Edges
correctEdges [] = []
correctEdges ((e,Start):es) = (Start,e) : correctEdges es
correctEdges ((End,e)  :es) = (e,End) : correctEdges es
correctEdges (e:es)         = e : correctEdges es 

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

makePath :: (Node, [Node]) -> [Path]
makePath (n, []) = []
makePath (n, x:xs) = [n,x] : makePath (n, xs)

isCorrectPath :: Path -> Bool
isCorrectPath (Start:[]) = False
isCorrectPath (Start: xs) = last xs == End
isCorrectPath          _  = False

edgeToPath :: (Node, Node) -> [Node]
edgeToPath (n, m) = [n,m]

getNext :: [(Node, Node)] -> (Node, Node) -> [[(Node, Node)]]
getNext xs (curr, End)  = [[(curr, End)]]
getNext xs (curr, next) = fs : concat (map (getNext xs) fs)
  where fs = filter (\(x,_) -> x == next) xs

buildPaths :: [(Node, Node)] -> (Node,Node) -> [(Node, Node)]
buildPaths xs (curr, next) = undefined 