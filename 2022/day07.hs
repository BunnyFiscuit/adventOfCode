{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day7 where
import FileReader
import Data.List

main :: IO ()
main = do
  contents <- readF' "test"
  print (parseLines contents)
  let fs = buildIt (parseLines contents) (Folder ["/"] 0 []) []
  print "hello!"

type Path = [String]
type Size = Int
type Subfolders = [Folder]
data Folder = Folder Path Size Subfolders deriving (Show, Read)

data Command = CD String | Back | LS deriving (Show, Read)
data Line    = C Command | I String deriving (Show, Read)

isItem :: Line -> Bool
isItem (I _) = True
isItem _     = False

parseCommand :: String -> Line
parseCommand "$ cd .."                   = C Back
parseCommand ('$':' ':'c':'d':' ': name) = C (CD name)
parseCommand "$ ls"                      = C LS
parseCommand s                           = I s

parseLines :: [String] -> [Line]
parseLines = map parseCommand

buildIt :: [Line] -> Folder -> Path -> Folder
buildIt []              f     path     = f
buildIt ((C (CD d)):xs) f@(Folder fname fsize subfolders) path = buildIt xs f (d:path)
buildIt ((C Back):xs)   f     (p:path) = buildIt xs f path
buildIt ((C LS):xs)     f     path     = buildIt xs f path
buildIt ((I ('d':'i':'r':' ':name)):xs) (Folder fname fsize subfolders) path 
  = buildIt xs (Folder fname fsize (Folder (name:path) 0 [] : subfolders)) path -- search and build 
buildIt ((I s):xs) (Folder fname fsize subfolders) path = buildIt xs (Folder fname (fsize + itemSize) subfolders) path
  where itemSize = read (takeWhile (/=' ') s) :: Int