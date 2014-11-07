{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Web.AWS.DynamoDB.ListTables
       ( -- * API
         listTables
         -- * Types
       , ListTables         (..)
       , ListTablesResponse (..)
       ) where

import           Control.Applicative     ( (<$>)
                                         , (<*>) )
import           Control.Monad           ( mzero )
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

test :: IO (Either DynamoError ListTablesResponse)
test = listTables $ ListTables Nothing Nothing

------------------------------------------------------------------------------
-- | Request Types
data ListTables = ListTables {
    exclusiveStartTableName :: Maybe Text
  , listTableRequestLimit   :: Maybe Int
  } deriving (Show)

------------------------------------------------------------------------------
-- | JSON Instances
instance ToJSON ListTables where
  toJSON ListTables{..} = object [
      "ExclusiveStartTableName" .= exclusiveStartTableName
    , "Limit"                   .= listTableRequestLimit
    ]

------------------------------------------------------------------------------
-- | Response Types
data ListTablesResponse = ListTablesResponse {
      lastEvaluatedTableName :: Maybe Text
    , tableNames             :: [Text]
  } deriving (Show)

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON ListTablesResponse where
  parseJSON (Object o) =
    ListTablesResponse <$> o .:? "LastEvalutedTableName"
                       <*> o .: "TableNames"
  parseJSON _ = mzero


