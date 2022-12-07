module Day7C where
import FileReader
import Data.List
import Data.Maybe

{-- 
data Item = Item {
  name :: !String,
  size :: !Int,
  itemType :: !ItemType,
  parent :: !String
} deriving (Show, Read)

data ItemType = Fold | File deriving (Show, Read)
--}

data Folder = Folder {
  parent :: String,
  name :: String,
  files :: [(Int, String)],
  folders :: [Folder]
} deriving (Show, Read)

initFolder parent name = Folder {
  parent = parent,
  name = name,
  files = [],
  folders = []
}

folderLookup :: Folder -> String -> Maybe Folder
folderLookup f lookupName
  | name f == lookupName = Just f
  | otherwise            = case folders f of
    [] -> Nothing
    xs -> if any isJust lookups then head lookups else Nothing
  where lookups = map (`folderLookup` lookupName) (folders f)


moveForward :: String -> String -> String
moveForward curr n = curr ++ "/" ++ n

moveBack :: String -> String
moveBack "/" = "/"
moveBack xs = reverse ( dropWhile (/='/') (reverse xs))

data Command = CD String | Back | LS deriving (Show, Read)
data Line    = C Command | I String deriving (Show, Read)

isItem :: Line -> Bool
isItem (I _) = True
isItem _     = False

parseCommand :: String -> Line
parseCommand "$ cd .."                  = C Back
parseCommand ('$':' ':'c':'d':' ':name) = C (CD name)
parseCommand "$ ls"                     = C LS
parseCommand s                          = I s

parseLines :: [String] -> [Line]
parseLines = map parseCommand

replace :: [Folder] -> Folder -> [Folder]
replace [] n     = []
replace (f:fs) n
  | name f == name n = n : fs
  | otherwise        = f { folders = replace (folders f) n} : replace (replace fs n) n 

parseItems :: [Line] -> Folder -> CurrentDir -> Folder
parseItems [] f _ = f
parseItems (C _ : xs) f dir = error "parseFolder - should not happen"
parseItems ((I s):xs) f dir = case chf of
    Nothing -> error "Sadness"
    Just f' -> if "dir" `isPrefixOf` s 
      then undefined
      else parseItems xs (head (replace [f] f')) dir
    where dirName    = drop 1 (dropWhile (/=' ') s)
          item       = (read sz :: Int, drop 1 name)
          (sz, name) = break (==' ') s
          chf        = folderLookup f [last dir]

type CurrentDir = String
type Dir        = String

createMainFolder :: [Line] -> Folder -> CurrentDir -> Folder
createMainFolder [] f _ = f
createMainFolder ((C (CD d)):xs) f dir
  | dir == d  = createMainFolder xs f dir
  | otherwise = createMainFolder xs ( f { folders = folders f ++ [initFolder dir d]}) (moveForward dir d)
createMainFolder ((C LS)    :xs) f dir = undefined
  where xs'   = dropWhile isItem xs
        items = takeWhile isItem xs
createMainFolder ((C Back)  :xs) f dir = createMainFolder xs f (moveBack dir)
createMainFolder ((I s)     :xs) f dir = error "How did this happen?"

main = do
  cs <- readF' "test"
  let ls = parseLines cs
  print ls
  let folder = createMainFolder ls (initFolder "root" "/") "/"
  print folder