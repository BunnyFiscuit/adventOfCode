{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day7 where
import FileReader
import Data.List
import Data.Maybe

main :: IO ()
main = do
  contents <- readF' "test"
  -- print (parseLines contents)
  let fs = buildIt (parseLines contents) (Folder ["/"] 0 []) []
  print fs

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
buildIt ((I ('d':'i':'r':' ':name)):xs) f path
  = buildIt xs (replace (newFolder (name:path) f) f) path -- search and build 
buildIt ((I s):xs) f path = buildIt xs (replace (addItem path f itemSize) f) path
  where itemSize = read (takeWhile (/=' ') s) :: Int

search :: Path -> Folder -> Maybe Folder
search [] f = Nothing
search path f@(Folder fname fsize subf)
  | concat path == concat fname           = Just f
  | concat fname `isSuffixOf` concat path = f'
  | otherwise                             = Nothing
  where mapSearch = filter isJust (map (search path) subf)
        f'        = case mapSearch of
          [] -> Nothing
          ss -> head ss

newFolder :: Path -> Folder -> Folder
newFolder path@(p:ps) f
  | isNothing searchResult = f
  | otherwise              = Folder fname fsize (Folder path 0 [] : sub)
  where searchResult = search ps f
        (Folder fname fsize sub) = fromJust searchResult

addItem :: Path -> Folder -> Size -> Folder
addItem p f s
  | isNothing searchResult = f
  | otherwise = Folder fname (fsize + s) sub
  where searchResult = search p f
        (Folder fname fsize sub) = fromJust searchResult

replace :: Folder -> Folder -> Folder
replace replacing@(Folder rname _ _) current@(Folder cname sz sub)
  | rname == cname   = replacing
  | concat cname `isSuffixOf` concat rname = Folder cname sz sub'
  | otherwise        = current
  where sub' = map (replace replacing) sub

f = Folder ["/"] 48381165 [Folder ["d","/"] 0 [], Folder ["a","/"] 0 [Folder ["e","a","/"] 0 []]]