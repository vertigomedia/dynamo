{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.Util
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.Util where

import           Data.Text              ( Text, pack )
import           Data.ByteString        ( ByteString )
import qualified Data.ByteString.Char8 as B8
import           Data.Time              ( UTCTime )
import           Data.Time.Clock.POSIX  ( posixSecondsToUTCTime )

------------------------------------------------------------------------------
-- | Text Helper
toText :: Show a => a -> Text
toText = pack . show

------------------------------------------------------------------------------
-- | ByteString Helper
toBS :: Show a => a -> ByteString
toBS = B8.pack . show

------------------------------------------------------------------------------
-- | Convert an `Integer` to a `UTCTime`
fromSeconds
  :: Integer
  -> UTCTime
fromSeconds = posixSecondsToUTCTime . fromInteger
