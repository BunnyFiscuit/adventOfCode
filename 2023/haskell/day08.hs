module Day where
import FileReader
import Data.List
import qualified Data.Map as M

type Path = (String, String)
type Node = String
type NodeMap = M.Map Node Path

main :: IO ()
main = do
  contents <- readF' "8"
  let instr = head contents
  --print $ instr
  let xs = drop 1 $ dropWhile (/="") contents
  let m = M.fromList $ map parseNode xs
  c <- run2 m instr -- part 1 is run, part 2 is run'
  print c

mm = M.fromList [("AAA",("BBB","BBB")),("BBB",("AAA","ZZZ")),("ZZZ",("ZZZ","ZZZ"))]

run :: NodeMap -> String -> IO Int
run m instr = run' m instr "AAA" 0
  where run' :: NodeMap -> String -> String -> Int -> IO Int
        run' m _ "ZZZ" c = return c
        run' m [] node c = run' m instr node c
        run' m (i:is) node c = case i of
              'L' -> run' m is (fst (m M.! node)) (c+1)
              'R' -> run' m is (snd (m M.! node)) (c+1)

-- use foldl1 lcm to find the least common multiple of a list of numbers
-- find the number of steps per node, previous result added to next.
-- foldl1 lcm [14429,20569,18727,22411,13201,18113] 
run2 :: NodeMap -> String -> IO Int
run2 m instr = do 
  cs <- mapM (\a -> run' m instr a 0) as
  print cs
  return $ sum cs
  where as = filter (\x -> 'A' == last x) (M.keys m)
        run' :: NodeMap -> String -> String -> Int -> IO Int
        run' m _ (_:_:'Z':[]) c = return c
        run' m [] node c = run' m instr node c
        run' m (i:is) node c = case i of
              'L' -> run' m is (fst (m M.! node)) (c+1)
              'R' -> run' m is (snd (m M.! node)) (c+1)
        run2' :: NodeMap -> String -> [String] -> Int -> IO Int
        run2' m [] ns c = run2' m instr ns c
        run2' m (i:is) ns c = case any p2end ns of
          True -> return c
          False -> do
            let ns' = map (lookupNext m i) ns
            run2' m is ns' (c+1)


lookupNext :: NodeMap -> Char -> String -> String
lookupNext m i node = case i of
    'L' -> fst (m M.! node)
    'R' -> snd (m M.! node)

p2end :: String -> Bool
p2end s = 'Z' == last s

parseNode :: String -> (Node, Path)
parseNode s = (n, parsePath p)
  where
    n = takeWhile (/=' ') s
    p = drop 2 $ dropWhile (/='=') s

parsePath :: String -> Path
parsePath s = (l, r)
  where l = take 3 $ drop 1 s
        r = take 3 $ drop 1 $ dropWhile (/=' ') s