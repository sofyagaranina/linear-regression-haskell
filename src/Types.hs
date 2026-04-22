module Types( Features(..), Label(..), DataPoint(..), Dataset(..),
   Regularization(..), HyperParams(..), LinearRegressionModel(..),
   Weights(..), Bias(..)) where

newtype Features = Features { featureList :: [Double] }
  deriving (Eq, Read, Show)

newtype Label = Label { labelValue :: Double }
  deriving (Eq, Read, Show)

data DataPoint = DataPoint{ pointFeatures :: Features, pointLabel :: Label}
  deriving (Eq, Read, Show)

newtype Dataset = Dataset { datasetPoints :: [DataPoint] }
  deriving (Eq, Read, Show)

data Regularization = NoReg | L1 | L2
  deriving (Eq, Read, Show)

data HyperParams = HyperParams{ learningRate :: Double, epochs :: Int, regularization :: Regularization, lambdaValue :: Double}
  deriving (Eq, Read, Show)

data LinearRegressionModel = LinearRegressionModel{ modelWeights :: [Double], modelBias :: Double, modelParams :: HyperParams}
  deriving (Eq, Read, Show)

newtype Weights = Weights { getWeights :: [Double]}
  deriving (Eq, Read, Show)

newtype Bias = Bias { getBias :: Double} 
  deriving (Eq, Read, Show)
