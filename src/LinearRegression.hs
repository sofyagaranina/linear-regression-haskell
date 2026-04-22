module LinearRegression (trainModel, predict) where

import Types

dotProduct :: [Double] -> [Double] -> Double
dotProduct xs ys = sum (zipWith (*) xs ys)

trainModel :: HyperParams -> Dataset -> LinearRegressionModel
trainModel params dataset =
  LinearRegressionModel finalWeights finalBias params
  where
    rows = datasetPoints dataset
    featureCount = getFeatureCount rows
    initialWeights = Weights (replicate featureCount 0.0)
    initialBias = Bias 0.0
    initialState = (initialWeights, initialBias)
    finalState = process initialState (epochs params)
    where process state 0 = state
          process state n = process (one_step state) (n-1)
    finalWeights = getWeights (fst finalState)
    finalBias = getBias (snd finalState)

    one_step :: (Weights, Bias) -> (Weights, Bias)
    one_step (weights, biasValue) =
      let rowCount = fromIntegral (length rows)
          gradientValues = gradients params dataset weights biasValue
          gradW = fst gradientValues
          gradB = snd gradientValues
          currentWeights = getWeights weights
          currentBias = getBias biasValue
          newWeights =
            zipWith
              (\weight gradient -> weight - (learningRate params) * gradient / rowCount)
              currentWeights
              gradW
          newBias = currentBias - (learningRate params) * gradB / rowCount
      in (Weights newWeights, Bias newBias)

predict :: LinearRegressionModel -> Features -> Double
predict model features = dotProduct (modelWeights model) (featureList features) + modelBias model

-- вычисляет градиент по весам и по смещению на всей выборке
gradients :: HyperParams -> Dataset -> Weights -> Bias -> ([Double], Double)
gradients params (Dataset rows) weights biasValue =
  (totalWeightGradient, totalBiasGradient)
  where
    currentWeights = getWeights weights
    currentBias = getBias biasValue
    initialWeightGradient = replicate (length currentWeights) 0.0
    initialBiasGradient = 0.0

    accGradients :: [DataPoint] -> [Double] -> Double -> ([Double], Double)
    accGradients [] weightGradient biasGradient = (weightGradient, biasGradient)
    accGradients (point : rest) weightGradient biasGradient =
      let features = featureList (pointFeatures point)
          realValue = labelValue (pointLabel point)
          predictedValue = dotProduct currentWeights features + currentBias
          errorValue = predictedValue - realValue
          weightGradientForPoint = map (* errorValue) features
          nextWeightGradient =
            zipWith
              (\oldGrad pointGrad -> oldGrad + pointGrad)
              weightGradient
              weightGradientForPoint
          nextBiasGradient = biasGradient + errorValue
      in accGradients rest nextWeightGradient nextBiasGradient

    dataGradient = accGradients rows initialWeightGradient initialBiasGradient
    regPartGradient = regGradient (regularization params) (lambdaValue params) weights
    totalWeightGradient = zipWith (+) (fst dataGradient) regPartGradient
    totalBiasGradient = snd dataGradient

-- градиент регуляризационного слагаемого
regGradient :: Regularization -> Double -> Weights -> [Double]
regGradient NoReg _ (Weights ws) = replicate (length ws) 0.0
regGradient L1 lambdaCoeff (Weights ws) = map (lambdaCoeff *) (map signum ws)
regGradient L2 lambdaCoeff (Weights ws) = map ((2.0 * lambdaCoeff) * ) ws

getFeatureCount :: [DataPoint] -> Int
getFeatureCount [] = 0
getFeatureCount ((DataPoint features label) : _) = length (featureList features)
