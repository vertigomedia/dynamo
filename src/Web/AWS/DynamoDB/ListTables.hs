{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Web.AWS.DynamoDB.ListTables
       ( -- * API
         listTables
         -- * Types
       , ListTables         (..)
       , ListTablesResponse (..)
       ) where

import Control.Monad
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
import           Web.AWS.DynamoDB.DeleteTable hiding (test)
import           Web.AWS.DynamoDB.Client

------------------------------------------------------------------------------
-- | Make Request
listTables :: ListTables -> IO (Either DynamoError ListTablesResponse)
listTables tables = callDynamo "ListTables" tables

listTablesDefault :: IO (Either DynamoError ListTablesResponse)
listTablesDefault = listTables $ ListTables Nothing Nothing

testDel :: IO ()
testDel = do Right names <- fmap tableNames <$> (listTables $ ListTables Nothing Nothing)
             xs <- forM names $ \n -> deleteTable (DeleteTable n)
             print xs
 
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


