{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}

module Main where

import Data.List
import System.Random
import List.Shuffle

main :: IO ()
main = do
    { g <- getStdGen
    ; n <- readLn
    ; let ss = take n $ unfoldr (phi n) g
    ; print ss
    }
    where
        phi n g = Just $ shuffle [1 .. n] g