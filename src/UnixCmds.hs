-- # UnixCmds
-- 
-- ## 言語拡張と`module`宣言
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module UnixCmds
    ( head5
    ) where

import Data.Char
import Data.List

type UnixCmd = [String] -> [String]

head5 :: UnixCmd
head5 = take 5

grepPattern :: UnixCmd
grepPattern = filter (isInfixOf "PATTERN")

egrepJanOrJul :: UnixCmd
egrepJanOrJul = filter (p ||| q) where
    p = isInfixOf "Jan"
    q = isInfixOf "Jul"
    (f ||| g) x = f x || g x

trLower2Upper :: UnixCmd
trLower2Upper = map (map toUpper)

wcl :: UnixCmd
wcl = singleton . show . length

seq1to10 :: UnixCmd
seq1to10 = const (map (show @Int) [1..10])


cutc1toc20 :: UnixCmd
cutc1toc20 = map (take 20)

interactWithEnv :: ((ProgName, Args, Envs) -> (String -> String)) -> IO ()
interactWithEnv = undefined

type ProgName = String
type Args     = [String]
type Envs     = [(String, String)]
