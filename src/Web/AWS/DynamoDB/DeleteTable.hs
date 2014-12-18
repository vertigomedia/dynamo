{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.DeleteTable
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.DeleteTable
    ( -- * Types
      DeleteTable (..)
    ) where

import           Data.Aeson              ( ToJSON   (..)
                                         , object
                                         , (.=) )
import           Data.Text               ( Text )
import           Data.Typeable           ( Typeable )

import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | `DeleteTable` type
data DeleteTable = DeleteTable {
    deleteTableName :: Text
  } deriving (Show, Eq, Typeable)

------------------------------------------------------------------------------
-- | `ToJSON` Instance for `DeleteTable`
instance ToJSON DeleteTable where
  toJSON DeleteTable{..} = object [ "TableName" .= deleteTableName ]

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction DeleteTable TableResponse


