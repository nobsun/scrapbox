module Shuffled where

import Data.List
import System.Random
import List.Shuffle


shuffled :: Int -> IO [[Int]]
shuffled n = take n . unfoldr phi <$> getStdGen where
    bs = [1 .. n]
    phi = Just . shuffle bs
