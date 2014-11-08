{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- |
-- Module      : Web.AWS.DynamoDB.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX

module Web.AWS.DynamoDB.Types where

import           Control.Applicative (pure, (<$>), (<*>))
import           Data.Text           (Text, pack, unpack, split)
import           Data.Maybe
import           Data.Time
import           Data.Char
import           Data.Aeson
import           Text.Read  hiding (String)

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Web.AWS.DynamoDB.Helpers

------------------------------------------------------------------------------
-- | Dyna-Monad :P
type DynamoDB = forall a . FromJSON a => EitherT String (ReaderT String IO) a

------------------------------------------------------------------------------
-- | Run Dynamo
--dynamo config requests = runReaderT config $ runEitherT requests

------------------------------------------------------------------------------
-- | DynamoDB Types: 
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModel.DataTypes>
data DynamoType =
    S    -- ^ String Type 
  | N    -- ^ Number Type
  | B    -- ^ Binary Type
  | BOOL -- ^ Boolean Type
  | NULL -- ^ Null Type
  | SS   -- ^ String set
  | NS   -- ^ Number set
  | BS   -- ^ Binary set
  | L    -- ^ List
  | M    -- ^ Map
  deriving (Show, Eq)

instance ToJSON DynamoType where
  toJSON = String . toText

instance FromJSON DynamoType where
   parseJSON (String "S") = pure S
   parseJSON (String "N") = pure N
   parseJSON (String "B") = pure B
   parseJSON (String "BOOL") = pure BOOL
   parseJSON (String "NULL") = pure NULL
   parseJSON (String "SS") = pure SS
   parseJSON (String "NS") = pure NS
   parseJSON (String "BS") = pure BS
   parseJSON (String "L") = pure L
   parseJSON (String "M") = pure M
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Attribute Defintions
data AttributeDefinition = AttributeDefinition {
      attributeName :: Text        -- ^ A name for the attribute, Minimum length of 1. Maximum length of 255, Required 
    , attributeType :: DynamoType  -- ^ Required, valid values : S | N | B
  } deriving (Show, Eq)

instance ToJSON AttributeDefinition where
  toJSON AttributeDefinition{..} =
    object [
        "AttributeName" .= attributeName
      , "AttributeType" .= attributeType
      ]

instance FromJSON AttributeDefinition where
   parseJSON (Object o) = 
     AttributeDefinition <$> o .: "AttributeName"
                         <*> o .: "AttributeType"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Key Type
data KeyType =
    HASH  -- ^ Hash Key Type
  | RANGE -- ^ Range Key Type
  deriving (Show, Eq)

instance ToJSON KeyType where
  toJSON = String . toText

instance FromJSON KeyType where
   parseJSON (String "HASH") = pure HASH
   parseJSON (String "Range") = pure RANGE
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Key Schema
data KeySchema = KeySchema {
      keySchemaAttributeName :: Text    -- ^ Required, Minimum length of 1. Maximum length of 255.
    , keySchemaType          :: KeyType -- ^ Required, HASH or RANGE
  } deriving (Show, Eq)

instance ToJSON KeySchema where
  toJSON KeySchema{..} = object [
      "AttributeName" .= keySchemaAttributeName
    , "KeyType" .= keySchemaType
    ]

instance FromJSON KeySchema where
   parseJSON (Object o) =
     KeySchema <$> o .: "AttributeName"
               <*> o .: "KeyType"
   parseJSON _ = mzero
 
------------------------------------------------------------------------------
-- | Provisioned ThroughPut
-- Represents the provisioned throughput settings for a specified
-- table or index. The settings can be modified using the UpdateTable operation.
data Throughput = Throughput {
     readCapacityUnits :: Int -- ^ Required, Long, The maximum number of strongly consistent reads consumed per second before DynamoDB returns a ThrottlingException
   , writeCapacityUnits :: Int -- ^ Required, Long, The maximum number of writes consumed per second before DynamoDB returns a ThrottlingException
  } deriving (Show, Eq)

instance ToJSON Throughput where
  toJSON Throughput{..} = 
    object [ "ReadCapacityUnits" .= readCapacityUnits
           , "WriteCapacityUnits" .= writeCapacityUnits
           ]

data ThroughputResponse = ThroughputResponse {
     readCapacityUnitsResp :: Int -- ^ Required, Long, The maximum number of strongly consistent reads consumed per second before DynamoDB returns a ThrottlingException
   , writeCapacityUnitsResp :: Int -- ^ Required, Long, The maximum number of writes consumed per second before DynamoDB returns a ThrottlingException
   , lastIncreaseDateTimeResp :: UTCTime
   , lastDecreaseDateTimeResp :: UTCTime
   , numberOfDecreasesTodayResp :: Int
  } deriving (Show, Eq)


instance FromJSON ThroughputResponse where
   parseJSON (Object o) =
     ThroughputResponse <$> o .: "ReadCapacityUnits"
                        <*> o .: "WriteCapacityUnits"
                        <*> (fromSeconds <$> o .: "LastIncreaseDateTime")
                        <*> (fromSeconds <$> o .: "LastDecreaseDateTime")
                        <*> o .: "NumberOfDecreasesToday"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Table Response JSON
data TableResponse = TableResponse {
    tableResponseAttributeDefintions    :: [AttributeDefinition]
  , tableResponseCreationTime           :: UTCTime
  , tableResponseGlobalSecondaryIndexes :: Maybe [GlobalSecondaryIndexResponse]
--  , tableResponseItemCount              :: Int
--  , tableResponseKeySchema              :: [KeySchema]
--  , tableResponseLocalSecondaryIndexes ::  Maybe [LocalSecondaryIndexResponse]
--  , tableResponseProvisionedThroughput  :: ThroughputResponse
--  , tableResponseName                   :: Text
--  , tableResponseSizeBytes              :: Int
--  , tableResponseStatus                 :: Int
  } deriving (Show)

instance FromJSON TableResponse where
  parseJSON (Object o) = do
    desc <- o .: "TableDescription"
    TableResponse <$> desc .: "AttributeDefinitions"
                  <*> (fromSeconds <$> desc .: "CreationDateTime")
                  <*> desc .:? "GlobalSecondaryIndexes"
--                  <*> desc .: "ItemCount"
--                  <*> desc .: "KeySchema"
--                  <*> desc .:? "LocalSecondaryIndexes"
--                  <*> desc .: "ProvisionedThroughput"
--                  <*> desc .: "TableName"
--                  <*> desc .: "TableSizeBytes"
--                  <*> desc .: "TableStatus"
  parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Item for insertion or retrieval
type ItemName = Text
type ItemValue = Text

data Item = Item ItemName DynamoType ItemValue

data Capacity =
    INDEXES
  | TOTAL -- NONE
   deriving (Show)

------------------------------------------------------------------------------
-- | Select on Query's
-- If not specified DynamoDB will default to ALL_Attributes
data Select =
    AllAttributes -- ^ Returns all of the item attributes from the specified table or index
  | AllProjectedAttributes -- ^ Only for querying an Index
  | Count -- ^ Returns the number of matching items rather than the matching items themselves
  | SpecificAttributes -- ^ Returns only the attributes listed in 'AttributesToGet'

instance ToJSON Select where
  toJSON AllAttributes = String "ALL_ATTRIBUTES"
  toJSON AllProjectedAttributes = String "ALL_PROJECTED_ATTRIBUTES"
  toJSON SpecificAttributes = String "SPECIFIC_ATTRIBUTES"
  toJSON Count = String "COUNT"

data ComparisonOperator =
  GT | LT | EQ | LE | GE | BEGINS_WITH | BETWEEN
  deriving (Show)

instance ToJSON ComparisonOperator where
  toJSON = String . toText 

------------------------------------------------------------------------------
-- | Primary Key
type KeyName = Text

data Key = Key { keyName :: KeyName
               , keyType :: DynamoType
               } deriving (Show, Eq)

data PrimaryKeyType = HashAndRangeType { hashKey :: Key, rangeKey :: Key }
                    | HashType { hashKey :: Key }
                      deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Local Secondary Index Request Type
data LocalSecondaryIndex = LocalSecondaryIndex {
      lsiIndexName :: Text
    , lsiProjection :: Projection
    , lsiKeySchema :: [KeySchema]
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance
instance ToJSON LocalSecondaryIndex where
  toJSON LocalSecondaryIndex{..} =
    object [ "IndexName" .= lsiIndexName
           , "KeySchema" .= lsiKeySchema
           , "Projection" .= lsiProjection
           ]

------------------------------------------------------------------------------
-- | Local Secondary Index Response Type
data LocalSecondaryIndexResponse = LocalSecondaryIndexResponse {
      lsiRespIndexName :: Text
    , lsiRespIndexSizeBytes :: Int
    , lsiRespItemCount :: Int
    , lsiRespProjection :: Projection
    , lsiRespKeySchema :: [KeySchema]
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance
instance FromJSON LocalSecondaryIndexResponse where
   parseJSON (Object o) =
     LocalSecondaryIndexResponse <$> o .: "IndexName"
                                 <*> o .: "IndexSizeBytes"
                                 <*> o .: "ItemCount"
                                 <*> o .: "KeySchema"
                                 <*> o .: "Projection"

------------------------------------------------------------------------------
-- | Global Secondary Indexes
data GlobalSecondaryIndexResponse = GlobalSecondaryIndexResponse {
    gsiRespIndexName      :: Text
  , gsiRespIndexSizeBytes :: Int
  , gsiRespIndexStatus    :: Status
  , gsiRespItemCount      :: Int
  , gsiRespKeySchema      :: [KeySchema]
  , gsiRespProjection     :: Projection
  , gsiRespProvisionedThroughput :: ThroughputResponse
  } deriving (Show, Eq)

instance FromJSON GlobalSecondaryIndexResponse where
   parseJSON (Object o) =
     GlobalSecondaryIndexResponse
        <$> o .: "IndexName"
        <*> o .: "IndexSizeBytes"
        <*> o .: "IndexStatus"
        <*> o .: "ItemCount"
        <*> o .: "KeySchema"
        <*> o .: "Projection"
        <*> o .: "ProvisionedThroughput"
   parseJSON _ = mzero

data GlobalSecondaryIndex = GlobalSecondaryIndex {
    gsiIndexName      :: Text
  , gsiKeySchema      :: [KeySchema]
  , gsiProjection     :: Projection
  , gsiProvisionedThroughput :: Throughput
  } deriving (Show, Eq)

instance ToJSON GlobalSecondaryIndex where
  toJSON GlobalSecondaryIndex{..} =
    object [ "IndexName" .= gsiIndexName
           , "KeySchema" .= gsiKeySchema
           , "Projection" .= gsiProjection
           , "ProvisionedThroughput" .= gsiProvisionedThroughput
           ]

------------------------------------------------------------------------------
-- | Status
data Status =
    CREATING
  | UPDATING
  | DELETING
  | ACTIVE
  | UnknownStatus
  deriving (Show, Eq, Read)

instance ToJSON Status where
  toJSON = String . toText

instance FromJSON Status where
   parseJSON (String x) =
     pure $ case readMaybe (unpack x) :: Maybe Status of
      Just x -> x
      Nothing -> UnknownStatus
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Projection
data Projection = Projection {
      nonKeyAttributes :: [Text]
    , projectionType   :: ProjectionType
  } deriving (Show, Eq)

instance ToJSON Projection where
  toJSON Projection{..} = do
    let proj = ["ProjectionType" .= projectionType]
    object $ case projectionType of
                 INCLUDE -> proj ++ [ "NonKeyAttributes" .= nonKeyAttributes ]
                 _       -> proj

instance FromJSON Projection where
   parseJSON (Object o) =
     Projection <$> o .: "NonKeyAttributes"
                <*> o .: "ProjectionType"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Projection Type
data ProjectionType =
    KEYS_ONLY -- ^ Only the index and primary keys are projected into the index
  | INCLUDE   -- ^ Only the specified table attributes are projected into the index. The list of projected
              -- attributes are in NonKeyAttributes
  | ALL       -- ^ All of the table attributes are projected into the index
  | UnknownProjectionType  -- ^ Not known
  deriving (Show, Eq, Read)

instance FromJSON ProjectionType where
   parseJSON (String x) =
     pure $ case readMaybe (unpack x) :: Maybe ProjectionType of
             Nothing -> UnknownProjectionType
             Just x -> x
   parseJSON _ = mzero

instance ToJSON ProjectionType where
  toJSON = String . toText

------------------------------------------------------------------------------
-- | Error Handling Types
type HTTPErrorCode = Int

data DynamoError =
    ProducerExhausted
  | ParseError
  | Err String
  | UnknownError String
  | ClientError HTTPErrorCode DynamoErrorDetails -- ^ Any error indicated by a 4xx response
  | ServerError HTTPErrorCode DynamoErrorDetails -- ^ Any error indicated by a 5xx response
  deriving (Show)

data DynamoErrorDetails = DynamoErrorDetails {
     dynamoErrorType    :: DynamoErrorType
   , dynamoErrorMessage :: Text
  } deriving (Show)

instance FromJSON DynamoErrorDetails where
   parseJSON (Object o) =
     DynamoErrorDetails <$> o .: "__type"
                        <*> o .: "Message"
   parseJSON _ = mzero

data DynamoErrorType =
    AccessDeniedException       -- ^ General authentication failure. The client did not correctly sign the request. Consult the signing documentation
  | IncompleteSignatureException -- ^ The request signature does not conform to AWS standards.
  | IncompleteSignature         -- ^ The request signature does not conform to AWS standards.
  | InternalFailure             -- ^ The request processing has failed because of an unknown error, exception or failure.
  | InvalidAction               -- ^ The action or operation requested is invalid. Verify that the action is typed correctly.
  | InvalidClientTokenId        -- ^ The X.509 certificate or AWS access key ID provided does not exist in our records
  | InvalidParameterCombination -- ^ Parameters that must not be used together were used together.
  | InvalidParameterValue       -- ^ An invalid or out-of-range value was supplied for the input parameter
  | InvalidQueryParameter       -- ^ The AWS query string is malformed or does not adhere to AWS standards
  | MalformedQueryString        -- ^ The query string contains a syntax error.
  | MissingAction               -- ^ The request is missing an action or a required parameter
  | MissingAuthenticationToken  -- ^ The request must contain either a valid (registered) AWS access key ID or X.509 certificate
  | MissingAuthenticationTokenException -- ^ The request must contain either a valid (registered) AWS access key ID or X.509 certificate
  | MissingParameter            -- ^ A required parameter for the specified action is not supplied.
  | OptInRequired               -- ^ The AWS access key ID needs a subscription for the service.
  | RequestExpired              -- ^ The request reached the service more than 15 minutes after the date stamp on the request or more
                                --   than 15 minutes after the request expiration date (such as for pre-signed URLs), or the date stamp
                                --   on the request is more than 15 minutes in the future.
  | ServiceUnavailable          -- ^ The request has failed due to a temporary failure of the server.
  | ResourceNotFoundException   -- ^ The resource which is being requested does not exist.
  | Throttling                  -- ^ The request was denied due to request throttling, This can be returned by the control plane API (CreateTable, DescribeTable, etc) when they are requested too rapidly.
  | ThrottlingException         -- ^ Rate of requests exceeds the allowed throughput.
  | ValidationError             -- ^ The input fails to satisfy the constraints specified by an AWS service.
  | ConditionalCheckFailedException -- ^ Example: The expected value did not match what was stored in the system.
  | ItemCollectionSizeExceededException -- ^ For a table with a local secondary index, a group of items with the same hash key has exceeded the maximum size limit of 10 GB.
  | LimitExceededException            -- ^ Too many operations for a given subscriber.
                                      -- Example: The number of concurrent table requests (cumulative number of tables in the
                                      -- CREATING, DELETING or UPDATING state)
                                      -- exceeds the maximum allowed of 10. The total limit of tables (currently in the ACTIVE state) is 250.
  | ProvisionedThroughputExceededException -- ^ You exceeded your maximum allowed provisioned throughput for a table or for one
                                           -- or more global secondary indexes. To view performance metrics for provisioned
                                           -- throughput vs. consumed throughput, go to the Amazon CloudWatch console.
  | ResourceInUseException      --  ^ The resource which you are attempting to change is in use. Example: You tried to recreate an existing table, or delete a table currently in the CREATING state.
  | UnrecognizedClientException -- ^ The Access Key ID or security token is invalid. The request signature is incorrect. The most likely cause is an invalid AWS access key ID or secret key.
  | ValidationException -- ^ This error can occur for several reasons, such as a required parameter that is missing,
                        -- a value that is out range, or mismatched data types.
                        -- The error message contains details about the specific part of the request that caused the error.
  | InternalServerError -- ^ The server encountered an internal error trying to fulfill the request.
  | ServiceUnavailableException -- ^ The server encountered an internal error trying to fulfill the request.
  | UnknownErrorType            -- ^ Customer unknown error type (not AWS specific)
   deriving (Show, Read)

------------------------------------------------------------------------------
-- | JSON instance
instance FromJSON DynamoErrorType where
   parseJSON (String x) = do
     let [_, t] = split (=='#') x
         typ    = unpack t
     pure $ case readMaybe typ :: Maybe DynamoErrorType of
      Just x  -> x
      Nothing -> UnknownErrorType
   parseJSON _ = mzero
