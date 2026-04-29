module Main (main) where

import System.Environment (getArgs)
import Lib (run)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [trainFile, testFile] -> do
            result <- run trainFile testFile Nothing
            case result of
                Left err   -> putStrLn ("Error: " ++ err)
                Right output -> putStrLn output
        [trainFile, testFile, modelFile] -> do
            result <- run trainFile testFile (Just modelFile)
            case result of
                Left err   -> putStrLn ("Error: " ++ err)
                Right output -> putStrLn output
        _ -> putStrLn "Usage:\n  ./program <train_file> <test_file>\n  ./program <train_file> <test_file> <model_file>"