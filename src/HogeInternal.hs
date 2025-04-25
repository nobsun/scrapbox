-- # HogeInternal
-- このファイルは`stack new`コマンドで自動的に`src/`に挿入されます
-- 
-- ## 言語拡張と`module`宣言
-- 最低限の指定をしてある
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE LambdaCase, MultiWayIf #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE DataKinds, PolyKinds, NoStarIsType, TypeFamilyDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot, NoFieldSelectors, DuplicateRecordFields #-}
module HogeInternal
    where

data H
    = K
    | S
    deriving (Eq, Show)

type Hs = [H]

newtype E a = E a
    deriving (Eq, Show)

e1 :: E Hs
e1 = E [K]

e2 :: E Hs -> E Hs -> E Hs
e2 (E s) (E t) = E ([S] ++ s ++ t)

