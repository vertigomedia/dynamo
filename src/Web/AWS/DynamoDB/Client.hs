{-# LANGUAGE RankNTypes        #-}
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
module Web.AWS.DynamoDB.Client where

import           Control.Concurrent.Async (async, waitCatch)
import           Control.Exception
import           Control.Retry
import           Control.Monad            ( when )
import           Data.Text                ( pack )
import           Control.Applicative
import           Data.Aeson

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Control.Monad.Trans.State.Strict
import           Data.Monoid
import           Data.Bool
import           Data.Time

import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import           Aws.SignatureV4
import           Aws.General hiding (parse)

import           Pipes.HTTP
import qualified Pipes.ByteString as PB 
import           Web.AWS.DynamoDB.Helpers
import           Pipes.Attoparsec    (parse) -- ParsingError(..)

import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Development Mode flag, Debug Flag
dev, debug :: Bool
dev   = False
debug = True

------------------------------------------------------------------------------
-- | Request issuer
callDynamo
  :: (ToJSON a, FromJSON b, Show b)
  => DynamoOp
  -> Manager
  -> PublicKey
  -> SecretKey
  -> a
  -> IO (Either DynamoError b)
callDynamo mgr op public secret bs = do
  let url = bool "https://dynamodb.us-east-1.amazonaws.com:443" "http://localhost:8000" dev
  req <- parseUrl url
  let rawjson = L.toStrict $ encode bs
  when debug $ print rawjson
  result <- createRequest rawjson op public secret
  case result of
    Left err -> return $ Left (RequestCreationError err)
    Right heads -> do
     let req' = req { method = "POST"
                    , requestHeaders = heads
                    , requestBody = stream $ PB.fromLazy (encode bs)
                    }
     let backOffPolicy = exponentialBackoff 5 
     res <- waitCatch =<< async (withHTTP req' mgr $ \resp -> 
              evalStateT (parse $ fromJSON <$> json') (responseBody resp))
     case res of
       Left e -> 
         case fromException e of
           Just (StatusCodeException (Status num _) headers _) -> do
             let res = lookup "X-Response-Body-Start" headers 
                 errorJson = case res of 
                    Nothing -> DynamoErrorDetails ClientParsingError "no json body"
                    Just x -> case eitherDecodeStrict x of
                                Left m -> DynamoErrorDetails ClientParsingError (pack m)
                                Right k -> k
             return . Left $ case num of
                        code | code >= 400 && code < 500 -> ClientError code errorJson
                             | code >= 500 -> ServerError code errorJson
           _ -> return $ Left $ UnknownError (show e)
       Right resp -> do
         when debug $ do putStrLn "after"
                         print resp
         return $ case resp of
           Nothing -> Left ParseError
           Just x ->  
             case x of
               Left _ -> Left ParseError
               Right y -> 
                 case y of
                   Success z -> Right z
                   Error g   -> Left $ Err g

createRequest
  :: ByteString
  -> DynamoOp
  -> PublicKey
  -> SecretKey
  -> IO (Either String RequestHeaders)
createRequest
  payload
  operation
  (PublicKey public)
  (SecretKey secret) = do
    creds <- newCredentials public secret
    now   <- getCurrentTime
    signPostRequestIO creds UsEast1 ServiceNamespaceDynamodb now "POST"
      ([] :: UriPath)
      ([] :: UriQuery)
      ( [ ("Accept-Encoding", "gzip")
      ,   ("connection", "Keep-Alive")
      ,   ("content-type", "application/x-amz-json-1.0")
      ,   ("Host", "dynamodb.us-east-1.amazonaws.com:443")
      ,   ("x-amz-target", "DynamoDB_20120810." <> toBS operation)
      ] :: RequestHeaders)
      payload





