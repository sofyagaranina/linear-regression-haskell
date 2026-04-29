module Lib (run) where

import DataLoader
import LinearRegression
import Metrics
import ModelSerializer
import Types
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)

run :: FilePath -> FilePath -> Maybe FilePath -> IO (Either String String)
run trainPath testPath modelPathOption = do
    -- Загрузка тестовых данных
    testResult <- loadData testPath
    case testResult of
        Left err -> return (Left err)
        Right testDataset -> do
            let testPoints = datasetPoints testDataset
            
            -- Получение модели в зависимости от режима
            modelResult <- case modelPathOption of
                Just modelPath -> do
                    putStrLn "Choose mode:"
                    putStrLn "  1 - Train new model and save to file"
                    putStrLn "  2 - Load existing model from file"
                    putStr "Enter choice (1 or 2): "
                    hFlush stdout
                    modeInput <- getLine
                    case modeInput of
                        "1" -> do
                            putStrLn $ "Training new model and saving to: " ++ modelPath
                            trainAndSave trainPath modelPath
                        "2" -> do
                            putStrLn $ "Loading model from: " ++ modelPath
                            loadAndTest modelPath testDataset
                        _ -> return (Left "Invalid choice. Please enter 1 or 2")
                Nothing -> do
                    putStrLn "No model file specified. Training new model (without saving)..."
                    trainOnly trainPath testDataset
            
            case modelResult of
                Left err -> return (Left err)
                Right (model, statusMessage, saveMessage) -> do
                    -- Предсказания
                    let correctAnswers = map getCorrectValue testPoints
                    let predictions = map (predictForPoint model) testPoints
                    let mseValue = mse correctAnswers predictions
                    
                    -- Вывод первых 10 предсказаний
                    let first10 = take 10 (zip correctAnswers predictions)
                        predictionsBlock = "  # | Actual | Predicted | Error\n" ++
                                          "----+--------+-----------+--------\n" ++
                                          unlines (map (\(i, (a, p)) -> 
                                              let iStr = show i
                                                  aStr = take 6 (show a ++ "      ")
                                                  pStr = take 9 (show p ++ "         ")
                                                  err = show (p - a)
                                              in "  " ++ iStr ++ " | " ++ aStr ++ " | " ++ pStr ++ " | " ++ err) 
                                              (zip [1::Int ..] first10))
                    
                    let params = modelParams model
                    let reg = regularization params
                        
                    let output = "Results\n" ++
                                 "Dataset Info" ++
                                 "Test samples: " ++ show (length testPoints) ++ "\n" ++
                                 "Number of features: " ++ show (length (modelWeights model)) ++ "\n" ++
                                 "Hyperparameters" ++
                                 "Learning rate: " ++ show (learningRate params) ++ "\n" ++
                                 "Epochs: " ++ show (epochs params) ++ "\n" ++
                                 "Regularization: " ++ show reg ++ "\n" ++
                                 "Lambda: " ++ show (lambdaValue params) ++ "\n" ++
                                 "MSE = " ++ show mseValue ++ "\n" ++
                                 "Predictions (first 10)" ++
                                 predictionsBlock
                    
                    return (Right output)

-- Обучение с нуля и сохранение
trainAndSave :: FilePath -> FilePath -> IO (Either String (LinearRegressionModel, String, String))
trainAndSave trainPath modelPath = do
    trainResult <- trainNewModel trainPath
    case trainResult of
        Left err -> return (Left err)
        Right (model, statusMessage) -> do
            saveResult <- saveModel modelPath model
            case saveResult of
                Right _ -> return (Right (model, statusMessage, "Model saved to: " ++ modelPath))
                Left err -> return (Left $ "Failed to save model: " ++ err)

-- Загрузка существующей модели
loadAndTest :: FilePath -> Dataset -> IO (Either String (LinearRegressionModel, String, String))
loadAndTest modelPath testDataset = do
    fileExists <- doesFileExist modelPath
    if not fileExists
        then return (Left $ "Model file not found: " ++ modelPath)
        else do
            loadResult <- loadModel modelPath
            case loadResult of
                Right model -> do
                    let msg = "Model loaded successfully from: " ++ modelPath
                    putStrLn msg
                    return (Right (model, msg, "Model was loaded (not saved)"))
                Left err -> return (Left err)

-- Режим без файла: только обучение
trainOnly :: FilePath -> Dataset -> IO (Either String (LinearRegressionModel, String, String))
trainOnly trainPath testDataset = do
    trainResult <- trainNewModel trainPath
    case trainResult of
        Left err -> return (Left err)
        Right (model, statusMessage) -> return (Right (model, statusMessage, "Model was not saved (no file specified)"))

-- Обучение 
trainNewModel :: FilePath -> IO (Either String (LinearRegressionModel, String))
trainNewModel trainPath = do
    trainingResult <- loadData trainPath
    case trainingResult of
        Left err -> return (Left err)
        Right trainingDataset -> do
            let trainPoints = datasetPoints trainingDataset
            let featureCount = if null trainPoints 
                then 0 
                else length (featureList (pointFeatures (head trainPoints)))
            
            putStrLn $ "Training data loaded. Samples: " ++ show (length trainPoints) ++ 
                      ", Features: " ++ show featureCount
            putStrLn "Enter hyperparameters:"
            
            putStr "Enter learning rate (default 0.01): "
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
            
            putStrLn "Model training completed."
            return (Right (model, "Model trained from scratch"))

getCorrectValue :: DataPoint -> Double
getCorrectValue point = labelValue (pointLabel point)

predictForPoint :: LinearRegressionModel -> DataPoint -> Double
predictForPoint model point = predict model (pointFeatures point)