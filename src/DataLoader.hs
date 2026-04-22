module DataLoader (loadData) where

import Types
import System.Directory (doesFileExist)
import Data.Char (isSpace)

loadData :: FilePath -> IO (Either String Dataset)
loadData path = do
    exists <- doesFileExist path
    if not exists
        then return (Left ("File not found: " ++ path))
        else do
            text <- readFile path
            let linesList = lines text
            let rows = parseLines linesList 1
            case rows of
                Left err -> return (Left err)
                Right points -> return (Right (Dataset points))

-- mapM применяет parseLine к каждой строке и собирает результаты в Either
-- если хоть один parseLine вернёт Left, mapM сразу вернёт эту ошибку
parseLines :: [String] -> Int -> Either String [DataPoint]
parseLines lines startLine =
    let indexedLines = zip [startLine..] lines
        nonEmptyLines = filter (\(_, line) -> not (null line) && not (all isSpace line)) indexedLines
    in mapM parseLine nonEmptyLines

parseLine :: (Int, String) -> Either String DataPoint
parseLine (lineNum, line) =
    let nums = map readDouble (words line)
    in
        if any isNothing nums
            then Left ("Line " ++ show lineNum ++ ": invalid number format")
        else if length nums < 2
            then Left ("Line " ++ show lineNum ++ ": need at least 1 feature and 1 label")
        else
            let values = map fromJust nums
            in Right (DataPoint (Features (init values)) (Label (last values)))

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

fromJust :: Maybe a -> a
fromJust (Just x) = x

readDouble :: String -> Maybe Double
readDouble s =
    case reads s of
        [(x, "")] -> Just x
        _ -> Nothing