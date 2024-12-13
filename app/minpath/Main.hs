{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Array
import MinPath
import Shuffled


size :: Int
size = 1000

main :: IO ()
main = do
    ij <- read <$> getLine
    print
     . flip minPath ij
     . listArray ((1,1),(size,size)) 
     . concat =<< shuffled size

