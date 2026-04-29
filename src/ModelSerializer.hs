module ModelSerializer (saveModel, loadModel) where

import Types
import System.Directory (doesFileExist)

saveModel :: FilePath -> LinearRegressionModel -> IO (Either String ())
saveModel path model = do
    let content = show model
    writeFile path content
    return (Right ())

loadModel :: FilePath -> IO (Either String LinearRegressionModel)
loadModel path = do
    exists <- doesFileExist path
    if not exists
        then return (Left $ "Model file not found: " ++ path)
        else do
            content <- readFile path
            case reads content of
                [(model, "")] -> return (Right model)
                _ -> return (Left "Failed to parse model file")