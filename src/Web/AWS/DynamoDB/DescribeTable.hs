{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.DescribeTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.DescribeTable
    ( -- * Types
      DescribeTable (..)
    ) where

import           Data.Aeson 
import           Data.Text    (Text)
import           Data.Typeable   

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types 

------------------------------------------------------------------------------
-- | `DescribeTable` object
data DescribeTable = DescribeTable {
    describeTableName :: Text
  } deriving (Show, Typeable, Eq)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `DescribeTable` 
instance ToJSON DescribeTable where
  toJSON DescribeTable{..} = object [ "TableName" .= describeTableName ]
       
------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction DescribeTable TableResponse
   

