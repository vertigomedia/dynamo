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
      DynamoAction         (..)
    , DynamoConfig         (..)
    , DynamoError          (..)
    , DynamoErrorDetails   (..)
    , DynamoErrorType      (..)
    , PublicKey            (..)
    , Region               (..)
    , SecretKey            (..)
      -- * BackOff Settings
    , module Control.Retry
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
import System.IO.Streams.HTTP
import Text.Printf         ( printf  )
import Text.Read  hiding   ( String, Number )

import Web.AWS.DynamoDB.Util

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
