-- # Hoge
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
module Hoge
    ( module H
    )
    where

import HogeInternal as H hiding (E)
import HogeInternal as H (E ())
