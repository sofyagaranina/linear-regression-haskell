module DataLoader (loadData) where

import Types
import System.Directory (doesFileExist)
import Data.Char (isSpace)

loadData :: FilePath -> IO (Either String Dataset)
loadData path = do
    -- doesFileExist :: FilePath -> IO Bool
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
parseLines fileLines startLine =
    let indexedLines = zip [startLine..] fileLines
        -- пустые строки пропускаем
        nonEmptyLines = filter (\(_, line) -> not (null line) && not (all isSpace line)) indexedLines
        results = mapM parseLine nonEmptyLines
    in case results of
        Left err -> Left err
        Right points ->
            let expectedCount = length (featureList (pointFeatures (head points)))
                allSame = all (\p -> length (featureList (pointFeatures p)) == expectedCount) points
            in if allSame
                then Right points
                else Left "All rows must have the same number of features"

parseLine :: (Int, String) -> Either String DataPoint
parseLine (lineNum, line) =
    let nums = map readDouble (words line)
    in
        if any (== Nothing) nums
            then Left ("Line " ++ show lineNum ++ ": invalid number format")
        else if length nums < 2
            then Left ("Line " ++ show lineNum ++ ": need at least 1 feature and 1 label")
        else
          -- с помощью спискового выражения вытаскиваем из контекста каждое значение
            let values = [x | Just x <- nums]
            in Right (DataPoint (Features (init values)) (Label (last values)))
readDouble :: String -> Maybe Double
readDouble s =
  -- reads пытается прочитать число из строки (возвращает список возможных пар:
  -- Первый элемент пары — прочитанное значение
  -- Второй элемент — остаток строки (то, что не прочиталось))
    case reads s of
        [(x, "")] -> Just x
        _ -> Nothing