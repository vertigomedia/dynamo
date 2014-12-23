{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--  
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.Client ( dynamo, defaultDynamoBackoffPolicy ) where

import           Control.Applicative            ( (<$>) )
import           Control.Exception              ( try, fromException )
import           Control.Monad                  ( when )
import           Control.Retry
import           Data.Text                      ( pack )
import           Data.Typeable                  ( typeOf )
import           Data.Aeson                     ( FromJSON
                                                , encode
                                                , json'
                                                , eitherDecodeStrict
                                                , Result (..)
                                                , fromJSON
                                                )
import qualified Data.ByteString.Lazy as L      
import           Data.Monoid                    ( (<>) )
import           Data.Bool                      ( bool )
import           Data.Time                      ( getCurrentTime )
import qualified Data.Text as T
import           Data.Text                      ( Text )
import           Network.HTTP.Client            ( Request (..) )
import           Network.HTTP.Types.Status      ( Status(..) )
import           Network.HTTP.Types.Header      ( RequestHeaders )
import           Aws.SignatureV4                ( newCredentials
                                                , signPostRequestIO
                                                , UriPath
                                                , UriQuery
                                                )
import           Aws.General hiding             ( parse, toText )
import           System.IO.Streams              ( InputStream, OutputStream)
import qualified System.IO.Streams as Streams   
import           System.IO.Streams.HTTP         ( parseUrl
                                                , method
                                                , requestHeaders
                                                , requestBody
                                                , stream
                                                , withHTTP
                                                , responseBody
                                                , HttpException ( StatusCodeException )
                                                )
import           System.IO.Streams.Attoparsec   ( parseFromStream )
                                                
import           Web.AWS.DynamoDB.Util          ( toBS )
import           Web.AWS.DynamoDB.Types         ( DynamoAction       (..)
                                                , DynamoConfig       (..)
                                                , DynamoError        (..)
                                                , DynamoErrorType    (..)
                                                , DynamoErrorDetails (..)
                                                , PublicKey          (..)
                                                , SecretKey          (..)
                                                )
import           Debug.Trace

------------------------------------------------------------------------------
-- | Default backoff policy, 5 tries, exponential at 500
defaultDynamoBackoffPolicy :: RetryPolicy
defaultDynamoBackoffPolicy = limitRetries 5 <> exponentialBackoff 500

------------------------------------------------------------------------------
-- | Request issuer
dynamo
  :: Show b
  => (DynamoAction a b)
  => DynamoConfig
  -> a
  -> IO (Either DynamoError b)
dynamo config@DynamoConfig{..} dynamoObject = do
  let produrl = "https://dynamodb." <> regionToText dynamoRegion <> ".amazonaws.com:443"
      testurl = "http://localhost:4567"
      url     = bool produrl testurl dynamoIsDev
      rawjson = L.toStrict $ encode dynamoObject
  when dynamoDebug $ print rawjson
  requestResult <- createRequest dynamoObject config
  case requestResult of
    Left err -> return . Left $ RequestCreationError err 
    Right heads -> retryDynamo (issueDynamo config dynamoObject url heads) dynamoBackOff

------------------------------------------------------------------------------
-- | Action to retry in case request throughput throttling occurs
-- issueDynamo
--   :: (DynamoAction a b)
--   => DynamoConfig
--   -> Text
--   -> a
--   -> IO (Either DynamoError b)
-- issueDynamo ::
--   (FromJSON b, ToJSON a) =>
--   DynamoConfig
--   -> a -> String -> RequestHeaders -> IO (Either DynamoError b)
issueDynamo config@DynamoConfig{..} dynamoObject url heads  = do
        req <- parseUrl url
        bsStr <- Streams.fromLazyByteString (encode dynamoObject)
        let req' = req {
            method = "POST"
          , requestHeaders = heads
          , requestBody = stream bsStr
        }
        result <- try $ withHTTP req' dynamoManager $ \resp -> do
                    r <- parseFromStream json' (responseBody resp)
                    return $ fromJSON r
        case result of
         Left e -> 
           case fromException e of
             Just (StatusCodeException (Status num _) headers _) -> do
               print $ lookup "X-Response-Body-Start" headers 
               let res = lookup "X-Response-Body-Start" headers 
                   errorJson = case res of 
                     Nothing -> DynamoErrorDetails ClientParsingError $ Just "no json body"
                     Just x  -> 
                       case eitherDecodeStrict x of
                         Left m  -> DynamoErrorDetails ClientParsingError $ Just (pack m)
                         Right k -> k
               return $ case num of
                 code | code >= 400 && code < 500 -> Left $ ClientError code errorJson
                      | code >= 500 -> Left $ ServerError code errorJson
         Right (Success resp) -> return $ Right resp
         Right (Error str)    -> return $ Left $ ParseError str

------------------------------------------------------------------------------
-- | Function to issue an http request to Dynamo over a specific
-- endpoint with a RetryPolicy that will kick in if the Provisioned
-- throughput for the table has exceeded. 
-- <http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#APIRetries>
retryDynamo
  :: Show t
  => IO (Either DynamoError t)
  -> RetryPolicy
  -> IO (Either DynamoError t)
retryDynamo action policy =
    retrying policy predicate action
  where
    predicate _ result = do
      return $ case result of
        (Left (ClientError 400 (DynamoErrorDetails ProvisionedThroughputExceededException _))) -> True
        (Left (ClientError 400 (DynamoErrorDetails LimitExceededException _))) -> True
        (Left (ClientError 400 (DynamoErrorDetails ResourceInUseException _))) -> True
        (Left (ClientError 400 (DynamoErrorDetails ThrottlingException _))) -> True
        (Left (ServerError 500 (DynamoErrorDetails InternalFailure _))) -> True
        (Left (ServerError 500 (DynamoErrorDetails InternalServerError _))) -> True
        (Left (ServerError 503 (DynamoErrorDetails ServiceUnavailableException _))) -> True
        otherwise -> False

------------------------------------------------------------------------------
-- | Generate AWS Headers
createRequest
  :: DynamoAction a b
  => a
  -> DynamoConfig
  -> IO (Either String RequestHeaders)
createRequest dynamoObject DynamoConfig{..} = do
      let PublicKey public = dynamoPublicKey
          SecretKey secret = dynamoSecretKey
          actionName       = toBS $ typeOf dynamoObject
      creds <- newCredentials public secret
      now   <- getCurrentTime
      signPostRequestIO creds dynamoRegion ServiceNamespaceDynamodb now "POST"
           ([] :: UriPath)
           ([] :: UriQuery)
           ( [ ("Accept-Encoding", "gzip")
           ,   ("connection", "Keep-Alive")
           ,   ("content-type", "application/x-amz-json-1.0")
           ,   ("Host", "dynamodb." <> regionToText dynamoRegion <> ".amazonaws.com:443")
           ,   ("x-amz-target", "DynamoDB_20120810." <> actionName)
             ] :: RequestHeaders)
           (L.toStrict $ encode dynamoObject)
