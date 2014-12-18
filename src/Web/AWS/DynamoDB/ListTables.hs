{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.ListTables
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.ListTables
       ( -- * API
         allTables
         -- * Types
       , ListTables         (..)
       , ListTablesResponse (..)
       ) where

import               Control.Applicative ( (<$>)
                                         , (<*>)
                                         )
import               Control.Monad       ( mzero )
import               Data.Aeson          ( FromJSON (..)
                                         , ToJSON   (..)
                                         , Value    (..)
                                         , object
                                         , (.=)
                                         , (.:?)
                                         , (.:)
                                         )
import               Data.Text           ( Text )
import               Data.Typeable       ( Typeable )

import               Web.AWS.DynamoDB.Types
import               Web.AWS.DynamoDB.Client

------------------------------------------------------------------------------
-- | Default ListTables value
allTables :: ListTables
allTables = ListTables Nothing Nothing

------------------------------------------------------------------------------
-- | Request Type
data ListTables = ListTables {
    exclusiveStartTableName :: Maybe Text
  , listTableRequestLimit   :: Maybe Int
  } deriving (Show, Typeable, Eq)

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

------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction ListTables ListTablesResponse



