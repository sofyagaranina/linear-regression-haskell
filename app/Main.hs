module Main (main) where

import System.Environment (getArgs)
import Lib (run)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2
        then putStrLn "Usage: ./program <train_file> <test_file>"
        else do
            result <- run (head args) (args !! 1)
            case result of
                Left err   -> putStrLn ("Error: " ++ err)
                Right output -> putStrLn output