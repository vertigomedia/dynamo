{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module      : Web.AWS.DynamoDB.ListTables
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.ListTables
       ( -- * API
         listTables
       , listTablesDefault
         -- * Types
       , ListTables         (..)
       , ListTablesResponse (..)
       ) where

import Control.Monad
import           Control.Applicative     ( (<$>)
                                         , (<*>) )
import           Data.Aeson              ( FromJSON (..)
                                         , ToJSON   (..)
                                         , Value    (..)
                                         , object
                                         , (.=)
                                         , (.:?)
                                         , (.:) )
import           Data.Text               ( Text )

import           Web.AWS.DynamoDB.Types
import           Web.AWS.DynamoDB.Client

------------------------------------------------------------------------------
-- | Make Request
listTables :: ListTables -> IO (Either DynamoError ListTablesResponse)
listTables tables = callDynamo "ListTables" tables

------------------------------------------------------------------------------
-- | Default Request for `ListTable` method
listTablesDefault :: IO (Either DynamoError ListTablesResponse)
listTablesDefault = listTables $ ListTables Nothing Nothing

------------------------------------------------------------------------------
-- | Request Type
data ListTables = ListTables {
    exclusiveStartTableName :: Maybe Text
  , listTableRequestLimit   :: Maybe Int
  } deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instances for `ListTables` object
instance ToJSON ListTables where
  toJSON ListTables{..} = object [
      "ExclusiveStartTableName" .= exclusiveStartTableName
    , "Limit"                   .= listTableRequestLimit
    ]

------------------------------------------------------------------------------
-- | Response Type
data ListTablesResponse = ListTablesResponse {
      lastEvaluatedTableName :: Maybe Text
    , tableNames             :: [Text]
  } deriving (Show)

------------------------------------------------------------------------------
-- | `FromJSON` instances for `ListTablesResponse` object
instance FromJSON ListTablesResponse where
  parseJSON (Object o) =
    ListTablesResponse <$> o .:? "LastEvalutedTableName"
                       <*> o .: "TableNames"
  parseJSON _ = mzero


