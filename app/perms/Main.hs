{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module Main where

import Data.List
import Data.Time.Clock
import Adwh.Perms

main :: IO ()
main = do
    { putStrLn sample
    ; t0 <- getCurrentTime
    ; print (length (permutations sample))
    ; t1 <- getCurrentTime
    ; print (length (perms1 sample))
    ; t2 <- getCurrentTime
    ; print (length (perms2 sample))
    ; t3 <- getCurrentTime
    ; let ts = [t3,t2,t1,t0]
    ; mapM_ print $ reverse $ zipWith diffUTCTime ts $ drop 1 ts
    }

sample :: String
sample = take 11 ['a' ..]
