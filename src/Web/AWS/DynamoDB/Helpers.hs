{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.Helpers
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.Helpers where

import           Data.Text    (Text, pack)

------------------------------------------------------------------------------
-- | Text Helper
toText :: Show a => a -> Text
toText = pack . show
