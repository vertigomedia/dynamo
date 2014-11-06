{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Web.AWS.DynamoDB.ListTables where

import           Control.Applicative
import           Data.Aeson
import           Data.Text    (Text)
import           Web.AWS.DynamoDB.Client
import           Web.AWS.DynamoDB.Types
import           Data.Time

------------------------------------------------------------------------------
-- | Make Request
listTables :: ListTables -> IO (Either String Value)
listTables = callDynamo "ListTables" 

test :: IO ()
test = print =<< (listTables $ ListTables Nothing Nothing)

------------------------------------------------------------------------------
-- | TableRespone
data TableResponse = TableResponse {
     tableResponseAttributeDefintions :: [AttributeDefinitions]
   , tableResponseCreationTime        :: UTCTime
  } deriving (Show)

------------------------------------------------------------------------------
-- | List Tables
data ListTables = ListTables {
    exclusiveStartTableName :: Maybe Text
  , listTableRequestLimit   :: Maybe Int
  } deriving (Show)

instance ToJSON ListTables where
  toJSON ListTables{..} = object [
      "ExclusiveStartTableName" .= exclusiveStartTableName
    , "Limit"                   .= listTableRequestLimit
    ]

data ListTableResponse = ListTableResponse {
      lastEvaluatedTableName :: Maybe Text
    , tableNames             :: Maybe [Text]
  } deriving (Show)

instance FromJSON ListTableResponse where
  parseJSON (Object o) =
    ListTableResponse <$> o .:? "LastEvalutedTableName"
                      <*> o .:? "TableNames"


