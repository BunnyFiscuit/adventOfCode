module Day7B where
import FileReader
import Data.List

data Folder = Folder {
  parent :: !String,
  name :: !String,
  files :: ![(Int, String)],
  folders :: ![String]
} deriving (Show, Read)

initFolder :: String -> Folder
initFolder name = Folder {
  parent = "",
  name = name,
  files = [],
  folders = []
}

main :: IO ()
main = do
  contents <- readF' "test"
  let c = filter (/= "$ cd ..") contents
  --print c
  let fds = groupFolders c
  --print fds
  let vs = validFolders fds fds
  --print vs
  let totSize = sum $ map (`folderSize` vs) vs
  print totSize

isItem = not . isPrefixOf "$"
isFile = not . isPrefixOf "dir"

groupFolders :: [String] -> [Folder]
groupFolders [] = []
groupFolders (x:xs)
  | dirName /= "_" = parsedFolder : groupFolders xs'
  | otherwise      = error $ "shouldn't happen i think" ++ show (dirName, x)
  where xs' = dropWhile isItem (drop 1 xs)
        dirItems = takeWhile isItem (drop 1 xs)
        dirName  = parseDirName x
        parsedFolder = parseFolder dirItems (initFolder dirName)

parseDirName :: String -> String
parseDirName ('$':' ':'c':'d':' ':name) = name
parseDirName _ = "_"

parseFolder :: [String] -> Folder -> Folder
parseFolder xs f = f { files = fs ++ files f, folders = ds ++ folders f }
  where fbFiles = map (break (==' ')) (filter (\x -> isItem x && isFile x ) xs)
        fs = map (\(sz, name) -> (read sz :: Int, drop 1 name)) fbFiles
        fds = filter (not . isFile) xs
        ds  = map (drop 1 . dropWhile (/= ' ')) fds

filterEmpty :: [Folder] -> [Folder]
filterEmpty = filter (not . null . files)

folderSize :: Folder -> [Folder] -> Int
folderSize f af = currentSize + innerSize
  where currentSize  = sum $ map fst (files f)
        innerFolders = [ getFolder af a | a <- folders f, folderSize (getFolder af a) af <= 100000]
        innerSize    = sum $ map (`folderSize` af) innerFolders

getFolder :: [Folder] -> String -> Folder
getFolder [] _ = initFolder "lol"
getFolder (f:fs) s
  | name f == s = f
  | otherwise   = getFolder fs s

validFolders :: [Folder] -> [Folder] -> [Folder]
validFolders [] _ = []
validFolders (f:fs) al
  | folderSize f al <= 100000 = f : validFolders fs al
  | otherwise                 = validFolders fs al