module Lib (run) where

import DataLoader
import LinearRegression
import Metrics
import Types
import System.IO (hFlush, stdout)

run :: FilePath -> FilePath -> IO (Either String String)
run trainPath testPath = do
    trainingResult <- loadData trainPath
    case trainingResult of
        Left err -> return (Left err)
        Right trainingDataset -> do
            testResult <- loadData testPath
            case testResult of
                Left err -> return (Left err)
                Right testDataset -> do
                    
                    putStr "Enter learning rate (default 0.01): "
                    -- принудительно из буфера на экран
                    hFlush stdout
                    lrInput <- getLine
                    let lr = if null lrInput then 0.01 else read lrInput
                    
                    putStr "Enter epochs (default 1000): "
                    hFlush stdout
                    epochsInput <- getLine
                    let epochsCount = if null epochsInput then 1000 else read epochsInput
                    
                    putStr "Enter regularization (NoReg/L1/L2) (default L1): "
                    hFlush stdout
                    regInput <- getLine
                    let reg = case regInput of
                            "NoReg" -> NoReg
                            "L2" -> L2
                            _ -> L1
                    
                    putStr "Enter lambda (default 0.01): "
                    hFlush stdout
                    lambdaInput <- getLine
                    let lambda = if null lambdaInput then 0.01 else read lambdaInput
                    
                    let params = HyperParams lr epochsCount reg lambda
                    let model = trainModel params trainingDataset
                    
                    let correctAnswers = map getCorrectValue (datasetPoints testDataset)
                    let predictions = map (predictForPoint model) (datasetPoints testDataset)
                    let mseValue = mse correctAnswers predictions
                    
                    let first10 = take 10 (zip correctAnswers predictions)
                        predictionsBlock = "  # | Actual | Predicted | Error\n" ++
                                          "----+--------+-----------+--------\n" ++
                                          unlines (map (\(i, (a, p)) -> 
                                              "  " ++ show i ++ " | " ++ 
                                              take 6 (show a ++ "      ") ++ " | " ++
                                              take 9 (show p ++ "         ") ++ " | " ++
                                              show (p - a)) (zip [1::Int,2,3,4,5,6,7,8,9,10] first10))
                    let output = "Results: " ++
                                 "Learning rate: " ++ show lr ++ "\n" ++
                                 "Epochs: " ++ show epochsCount ++ "\n" ++
                                 "Regularization: " ++ show reg ++ "\n" ++
                                 "Lambda: " ++ show lambda ++ "\n" ++
                                 "MSE = " ++ show mseValue ++ "\n" ++
                                 predictionsBlock
                    
                    return (Right output)

getCorrectValue :: DataPoint -> Double
getCorrectValue point = labelValue (pointLabel point)

predictForPoint :: LinearRegressionModel -> DataPoint -> Double
predictForPoint model point = predict model (pointFeatures point)