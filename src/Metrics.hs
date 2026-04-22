module Metrics (mse) where

mse :: [Double] -> [Double] -> Double
mse goal predicted = (sum squaredErrors) / (fromIntegral (length goal))
  where squaredErrors = zipWith (\ g p -> (g - p) ^ (2 :: Int)) goal predicted
