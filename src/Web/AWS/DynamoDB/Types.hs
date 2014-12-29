{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE OverloadedStrings      #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- 
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.Types
    ( -- * Types
      AttributeDefinition  (..)
    , DynamoAction         (..)
    , DynamoConfig         (..)
    , DynamoError          (..)
    , DynamoErrorDetails   (..)
    , DynamoErrorType      (..)
    , DynamoType           (..)
    , GlobalSecondaryIndex (..)
    , Key                  (..)
    , KeySchema            (..)
    , KeyType              (..)
    , LocalSecondaryIndex  (..)
    , PublicKey            (..)
    , Region               (..)
    , SecretKey            (..)
    , Throughput           (..)
    , TableResponse        (..)
    , Capacity             (..)
    , Select               (..)
    , ReturnValue          (..)
    , ComparisonOperator   (..)
    , PrimaryKey           (..)
    , Name
      -- * Helper functions                           
    , keyToAttribute 
    , keyToKeySchema
      -- * BackOff Settings
    , module Control.Retry
    , module Network.HTTP.Client
    , module Network.HTTP.Client.TLS
    , module Web.AWS.DynamoDB.Abstract
    ) where

import Aws.General         ( Region (..) )
import Control.Applicative ( pure, (<$>), (<*>), (<|>) )
import Control.Monad       ( forM, mzero, liftM )
import Control.Retry       ( RetryPolicy )
import Data.Aeson
import Data.Aeson.Types    ( typeMismatch )
import Data.ByteString     ( ByteString )
import Data.Maybe          ( fromMaybe  )
import Data.Text           ( Text, unpack, split )
import Data.Time           ( UTCTime  )
import Data.Typeable       ( Typeable )
import Data.Scientific
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M
import Data.HashMap.Strict ( toList  )
import Network.HTTP.Client 
import Network.HTTP.Client.TLS 
import Text.Printf         ( printf  )
import Text.Read  hiding   ( String, Number )

import Web.AWS.DynamoDB.Util
import Web.AWS.DynamoDB.Abstract

------------------------------------------------------------------------------
-- | Dynamo Config Type, stores Dynamo DB settings and connection manager (pool)
data DynamoConfig = DynamoConfig {
        dynamoPublicKey :: PublicKey
      , dynamoSecretKey :: SecretKey
      , dynamoManager   :: Manager
      , dynamoBackOff   :: RetryPolicy
      , dynamoRegion    :: Region
      , dynamoIsDev     :: Bool -- * Default port for dynalite is 4567
      , dynamoDebug     :: Bool -- * Will print json response and http info
      } 

------------------------------------------------------------------------------
-- | Show instance for DynamoConfig
instance Show DynamoConfig where
    show (DynamoConfig pk sk _ _ reg dev debug) =
        "== Dynamo Config ==\n" ++
        concat [ " "
               , show pk
               , "\n "
               , show sk
               , "\n "
               , show reg
               , " debug: "
               , show dev
               , "\n dev: "
               , show debug
               ]

------------------------------------------------------------------------------
-- | A typeclass to constrain what kinds of requests can be made to
-- the dynamo endpoint
class (Typeable a, ToJSON a, FromJSON b) => DynamoAction a b | a -> b

------------------------------------------------------------------------------
-- | AWS Public Key
newtype PublicKey = PublicKey ByteString
    deriving (Show, Eq)

------------------------------------------------------------------------------
-- | AWS Public Key
newtype SecretKey = SecretKey ByteString
    deriving (Show, Eq)

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

------------------------------------------------------------------------------
-- | `ToJSON` instance for `DynamoType`
instance ToJSON DynamoType where
  toJSON = String . toText

------------------------------------------------------------------------------
-- | `FromJSON` instance for `DynamoType`
instance FromJSON DynamoType where
   parseJSON (String "S")    = pure S
   parseJSON (String "N")    = pure N
   parseJSON (String "B")    = pure B
   parseJSON (String "BOOL") = pure BOOL
   parseJSON (String "NULL") = pure NULL
   parseJSON (String "SS")   = pure SS
   parseJSON (String "NS")   = pure NS
   parseJSON (String "BS")   = pure BS
   parseJSON (String "L")    = pure L
   parseJSON (String "M")    = pure M
   parseJSON val = typeMismatch "incorrect type for DynamoType" val

------------------------------------------------------------------------------
-- | Attribute Defintions
data AttributeDefinition = AttributeDefinition {
      attributeName :: Text        -- ^ A name for the attribute, Minimum length of 1. Maximum length of 255, Required 
    , attributeType :: DynamoType  -- ^ Required, valid values : S | N | B
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | A Key-Value pair
data Attribute = Attribute {
      attrName :: T.Text
    , attrVal  :: DValue
    } deriving (Read,Show,Ord,Eq,Typeable)
               
------------------------------------------------------------------------------
-- | `ToJSON` instance for `AttributeDefinition`
instance ToJSON AttributeDefinition where
  toJSON AttributeDefinition{..} =
    object [
        "AttributeName" .= attributeName
      , "AttributeType" .= attributeType
      ]

------------------------------------------------------------------------------
-- | `FromJSON` instance for `AttributeDefinition`
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

------------------------------------------------------------------------------
-- | `ToJSON` instance for `KeyType`
instance ToJSON KeyType where
  toJSON = String . toText

------------------------------------------------------------------------------
-- | `FromJSON` instance for `KeyType`
instance FromJSON KeyType where
   parseJSON (String "HASH")  = pure HASH
   parseJSON (String "RANGE") = pure RANGE
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Key Schema
data KeySchema = KeySchema {
      keySchemaAttributeName :: Text       -- ^ Required, Minimum length of 1. Maximum length of 255.
    , keySchemaType          :: KeyType    -- ^ Required, HASH or RANGE
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Key to KeySchema conversion
keyToKeySchema :: Key -> KeySchema
keyToKeySchema (Key x y _) = KeySchema x y

------------------------------------------------------------------------------
-- | Key to Attribute Conversion
keyToAttribute :: Key -> AttributeDefinition
keyToAttribute (Key x _ z) = AttributeDefinition x z

------------------------------------------------------------------------------
-- | A User-level abstraction for Dynamo Keys
data Key = Key {
      keyName       :: Text       -- ^ Required, Minimum length of 1. Maximum length of 255.
    , keyType       :: KeyType    -- ^ Required, HASH or RANGE
    , keyDynamoType :: DynamoType -- ^ The Dynamo type for this key (S | B | N) are the only valid types
    } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `KeySchema`
instance ToJSON KeySchema where
  toJSON KeySchema{..} = object [
      "AttributeName" .= keySchemaAttributeName
    , "KeyType"       .= keySchemaType
    ]

------------------------------------------------------------------------------
-- | `FromJSON` instance for `KeySchema`
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
     readCapacityUnits :: Integer -- ^ Required, Long, The maximum number of strongly consistent reads consumed per second before DynamoDB returns a ThrottlingException
   , writeCapacityUnits :: Integer -- ^ Required, Long, The maximum number of writes consumed per second before DynamoDB returns a ThrottlingException
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `Throughput`
instance ToJSON Throughput where
  toJSON Throughput{..} = 
    object [ "ReadCapacityUnits" .= readCapacityUnits
           , "WriteCapacityUnits" .= writeCapacityUnits
           ]

------------------------------------------------------------------------------
-- | `ThroughputResponse` object
data ThroughputResponse = ThroughputResponse {
     readCapacityUnitsResp      :: Integer -- ^ Required, Long, The maximum number of strongly consistent reads consumed per second before DynamoDB returns a ThrottlingException
   , writeCapacityUnitsResp     :: Integer -- ^ Required, Long, The maximum number of writes consumed per second before DynamoDB returns a ThrottlingException
   , lastIncreaseDateTimeResp   :: Maybe UTCTime
   , lastDecreaseDateTimeResp   :: Maybe UTCTime
   , numberOfDecreasesTodayResp :: Maybe Integer
  } deriving Eq

------------------------------------------------------------------------------
-- | ThroughputResponse type
instance Show ThroughputResponse where
    show ThroughputResponse{..} =
      printf "Read-Write Capacity: %d - %d" readCapacityUnitsResp writeCapacityUnitsResp

------------------------------------------------------------------------------
-- | `FromJSON` instance for `ThroughputResponse`i
instance FromJSON ThroughputResponse where
   parseJSON (Object o) =
     ThroughputResponse <$> o .: "ReadCapacityUnits"
                        <*> o .: "WriteCapacityUnits"
                        <*> (fmap fromSeconds <$> o .:? "LastIncreaseDateTime")
                        <*> (fmap fromSeconds <$> o .:? "LastDecreaseDateTime")
                        <*> o .:? "NumberOfDecreasesToday"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | TableResponse object
data TableResponse = TableResponse {
    tableResponseAttributeDefintions    :: Maybe [AttributeDefinition]
  , tableResponseCreationTime           :: Maybe UTCTime
  , tableResponseGlobalSecondaryIndexes :: Maybe [GlobalSecondaryIndexResponse]
  , tableResponseKeySchema              :: Maybe [KeySchema]
  , tableResponseLocalSecondaryIndexes  :: Maybe [LocalSecondaryIndexResponse]
  , tableResponseItemCount              :: Integer
  , tableResponseProvisionedThroughput  :: ThroughputResponse
  , tableResponseName                   :: Text
  , tableResponseSizeBytes              :: Integer
  , tableResponseStatus                 :: Status
  } 

------------------------------------------------------------------------------
-- | `FromJSON` instance for `TableResponse`
instance FromJSON TableResponse where
  parseJSON (Object o) = do
    desc <- (o .: "TableDescription" <|> o .: "Table")
    TableResponse <$> desc .:? "AttributeDefinitions"
                  <*> (fmap fromSeconds <$> desc .:? "CreationDateTime")
                  <*> desc .:? "GlobalSecondaryIndexes"
                  <*> desc .:? "KeySchema"
                  <*> desc .:? "LocalSecondaryIndexes"
                  <*> desc .: "ItemCount"
                  <*> desc .: "ProvisionedThroughput"
                  <*> desc .: "TableName"
                  <*> desc .: "TableSizeBytes"
                  <*> desc .: "TableStatus"
  parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Show instnace for `TableResponse`
instance Show TableResponse where
  show TableResponse {..} = 
         printf "\n\
                \TableName  : %s\n\
                \TableStatus: %s\n\
                \ItemCount  : %d\n\
                \Throughput : %s\n\
                \SizeBytes  : %d\n\
                \Attributes : %s\n\
                \Creation   : %s\n\
                \KeySchema  : %s\n\
                \LSI        : %s\n\
                \GSI        : %s\n"
         (unpack tableResponseName)
         (show tableResponseStatus)
         tableResponseItemCount
         (show tableResponseProvisionedThroughput)
         (tableResponseSizeBytes)
         (show tableResponseAttributeDefintions)
         (show tableResponseCreationTime)
         (show tableResponseKeySchema)
         (show tableResponseLocalSecondaryIndexes)
         (show tableResponseGlobalSecondaryIndexes)

------------------------------------------------------------------------------
-- | Item for insertion or retrieval
type Name = Text

------------------------------------------------------------------------------
-- | Name for a Table
newtype TableName = TableName { unTable :: Text }
   deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Capacity `Item`
data Capacity =
    INDEXES
  | TOTAL
--  | NONE
   deriving (Show)

------------------------------------------------------------------------------
-- | ReturnValue
data ReturnValue = NONE | ALL_OLD | UPDATED_OLD | ALL_NEW | UPDATED_NEW
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | ToJSON Instance for `ReturnValue`
instance ToJSON ReturnValue where
  toJSON = String . toText

------------------------------------------------------------------------------
-- | Select on Query's
-- If not specified DynamoDB will default to ALL_Attributes
data Select =
    AllAttributes          -- ^ Returns all of the item attributes from the specified table or index
  | AllProjectedAttributes -- ^ Only for querying an Index
  | Count                  -- ^ Returns the number of matching items rather than the matching items themselves
  | SpecificAttributes     -- ^ Returns only the attributes listed in 'AttributesToGet'
    deriving (Show)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `Select` 
instance ToJSON Select where
  toJSON AllAttributes          = String "ALL_ATTRIBUTES"
  toJSON AllProjectedAttributes = String "ALL_PROJECTED_ATTRIBUTES"
  toJSON SpecificAttributes     = String "SPECIFIC_ATTRIBUTES"
  toJSON Count                  = String "COUNT"

------------------------------------------------------------------------------
-- | `ComparisonOperator` object
data ComparisonOperator =
  DGT | DLT | DEQ | DLE | DGE | BEGINS_WITH | BETWEEN
  
instance Show ComparisonOperator where
    show DGT = "GT"
    show DLT = "LT"
    show DEQ = "EQ"
    show DLE = "LE"
    show DGE = "GE"
    show BEGINS_WITH = "BEGINS_WITH"
    show BETWEEN = "BETWEEN"

------------------------------------------------------------------------------
-- | `ToJSON` instance for `ComparisonOperator` 
instance ToJSON ComparisonOperator where
  toJSON = String . toText 

------------------------------------------------------------------------------
-- | Primary Key
type KeyName = Text

------------------------------------------------------------------------------
-- | `PrimaryKeyType` object
data PrimaryKey = PrimaryKey {
        hashKey :: (Text, DValue)
      , rangeKey :: Maybe (Text, DValue)
     } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | ToJSON Primary Key
instance ToJSON PrimaryKey where
  toJSON PrimaryKey{..} =
      let (k,v) = hashKey
      in case rangeKey of
           Just (k2,v2) ->
               object [
                 (k, toJSON v)
               , (k2, toJSON v2)
               ]
           Nothing-> object [ (k, toJSON v) ]

------------------------------------------------------------------------------
-- | Local Secondary Index Request Type
-- Local Secondary indices are not allowed on hash tables, only hash and range tables
data LocalSecondaryIndex = LocalSecondaryIndex {
      lsiIndexName  :: Text
    , lsiProjection :: Projection
    , lsiKeySchema  :: [Key]
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `ToJSON` Instance for `LocalSecondaryIndex`
instance ToJSON LocalSecondaryIndex where
  toJSON LocalSecondaryIndex{..} =
    object [ "IndexName"  .= lsiIndexName
           , "KeySchema"  .= map keyToKeySchema lsiKeySchema
           , "Projection" .= lsiProjection
           ]

------------------------------------------------------------------------------
-- | Local Secondary Index Response Type
data LocalSecondaryIndexResponse = LocalSecondaryIndexResponse {
      lsiRespIndexName      :: Text
    , lsiRespIndexSizeBytes :: Integer
    , lsiRespItemCount      :: Integer
    , lsiRespProjection     :: Projection
    , lsiRespKeySchema      :: [KeySchema]
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instance
instance FromJSON LocalSecondaryIndexResponse where
   parseJSON (Object o) =
     LocalSecondaryIndexResponse <$> o .: "IndexName"
                                 <*> o .: "IndexSizeBytes"
                                 <*> o .: "ItemCount"
                                 <*> o .: "Projection"
                                 <*> o .: "KeySchema"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Global Secondary Index Response object
data GlobalSecondaryIndexResponse = GlobalSecondaryIndexResponse {
    gsiRespIndexName      :: Text
  , gsiRespIndexSizeBytes :: Integer
  , gsiRespIndexStatus    :: Status
  , gsiRespItemCount      :: Integer
  , gsiRespKeySchema      :: [KeySchema]
  , gsiRespProjection     :: Projection
  , gsiRespProvisionedThroughput :: ThroughputResponse
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `FromJSON` instance for `GlobalSecondaryIndexeResponse`
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

------------------------------------------------------------------------------
-- | `GlobalSecondaryIndex` object
data GlobalSecondaryIndex = GlobalSecondaryIndex {
    gsiIndexName             :: Text
  , gsiProjection            :: Projection
  , gsiProvisionedThroughput :: Throughput
  , gsiKeySchema             :: [Key]
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `GlobalSecondaryIndex`
instance ToJSON GlobalSecondaryIndex where
  toJSON GlobalSecondaryIndex{..} =
    object [ "IndexName"             .= gsiIndexName
           , "Projection"            .= gsiProjection
           , "ProvisionedThroughput" .= gsiProvisionedThroughput
           , "KeySchema"             .= map keyToKeySchema gsiKeySchema
           ]

------------------------------------------------------------------------------
-- | `Status` object
data Status =
    CREATING
  | UPDATING
  | DELETING
  | ACTIVE
  | UnknownStatus
  deriving (Show, Eq, Read)

------------------------------------------------------------------------------
-- | `ToJSON` instance for `Status`
instance ToJSON Status where
  toJSON = String . toText

------------------------------------------------------------------------------
-- | `FromJSON` instance for `Status`
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

------------------------------------------------------------------------------
-- | `ToJSON` instance for `Projection`
instance ToJSON Projection where
  toJSON Projection{..} = do
    let proj = ["ProjectionType" .= projectionType]
    object $ case projectionType of
                 INCLUDE -> proj ++ [ "NonKeyAttributes" .= nonKeyAttributes ]
                 _       -> proj

------------------------------------------------------------------------------
-- | `FromJSON` instance for `Projection`
instance FromJSON Projection where
   parseJSON (Object o) =
     (Projection <$> o .: "NonKeyAttributes"
                 <*> o .: "ProjectionType")
                 <|>
     (Projection <$> pure []
                 <*> o .: "ProjectionType")
     
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

------------------------------------------------------------------------------
-- | `FromJSON` instance for `ProjectionType`
instance FromJSON ProjectionType where
   parseJSON (String "KEYS_ONLY") = pure KEYS_ONLY
   parseJSON (String "INCLUDE")   = pure INCLUDE
   parseJSON (String "ALL")       = pure ALL
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `ToJSON` instance for `ProjectionType`
instance ToJSON ProjectionType where
  toJSON = String . toText

------------------------------------------------------------------------------
-- | Error Handling Types
type HTTPErrorCode = Int

------------------------------------------------------------------------------
-- | `DynamoError` object
data DynamoError =
    ParseError String
  | RequestCreationError String
  | ConnectionError String
  | ClientError HTTPErrorCode DynamoErrorDetails -- ^ Any error indicated by a 4xx response
  | ServerError HTTPErrorCode DynamoErrorDetails -- ^ Any error indicated by a 5xx response
  deriving (Show)

------------------------------------------------------------------------------
-- | `DynamoErrorDetails` object
data DynamoErrorDetails = DynamoErrorDetails {
     dynamoErrorType    :: DynamoErrorType
   , dynamoErrorMessage :: Maybe Text
  } deriving (Show)

------------------------------------------------------------------------------
-- | `FromJSON` instance for `DynamoErrorDetails` object
instance FromJSON DynamoErrorDetails where
   parseJSON (Object o) =
     DynamoErrorDetails <$> o .: "__type"
                             -- Amazon's json is very inconsistent to say the least
                        <*> (o .:? "message" <|> o .:? "Message")
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `DynamoErrorType` object
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
  | SerializationException
  | ClientParsingError -- ^ Client couldn't parse the error json
  | UnknownErrorType            -- ^ Customer unknown error type (not AWS specific)
   deriving (Show, Read)

------------------------------------------------------------------------------
-- | `FromJSON` instance for `DynamoErrorType`
instance FromJSON DynamoErrorType where
   parseJSON (String x) = do
     let [_, t] = split (=='#') x
         typ    = unpack t
     pure $ fromMaybe UnknownErrorType (readMaybe typ :: Maybe DynamoErrorType)
   parseJSON _ = mzero
