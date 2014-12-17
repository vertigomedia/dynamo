{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- | 
-- Module      : Web.AWS.DynamoDB.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- | 
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.Client where

import           Control.Exception
import           Control.Monad       ( when, (>=>) )
import           Data.Text           ( pack )
import           Control.Applicative
import           Data.Aeson

import           Data.ByteString     ( ByteString )
import qualified Data.ByteString.Lazy as L
import           Control.Monad.Trans.State.Strict
import           Data.Monoid
import           Data.Bool
import           Data.Time

import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header
import           Aws.SignatureV4
import           Aws.General hiding  ( parse )

import           System.IO.Streams ( InputStream, OutputStream) 
import qualified System.IO.Streams as Streams
import           System.IO.Streams.HTTP
import           System.IO.Streams.Attoparsec 
import           System.IO.Streams.ByteString

import           Web.AWS.DynamoDB.Helpers
import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Operation Type
type Operation = ByteString

------------------------------------------------------------------------------
-- | Development Mode flag, Debug Flag
dev, debug :: Bool
dev   = False
debug = True

------------------------------------------------------------------------------
-- | Request issuer
callDynamo
  :: (ToJSON a, FromJSON b, Show b)
  => Operation
  -> a
  -> IO (Either DynamoError b)
callDynamo op bs = do
  let url = bool "https://dynamodb.us-east-1.amazonaws.com:443" "http://localhost:8000" dev
      rawjson = L.toStrict $ encode bs
  when debug $ print rawjson
  req <- parseUrl url
  Right heads <- createRequest rawjson op
  bsStr <- Streams.fromLazyByteString (encode bs)
  let req' = req {
       method = "POST"
     , requestHeaders = heads
     , requestBody = stream bsStr
    }
  result <- try $ withManager tlsManagerSettings $ \m -> 
           withHTTP req' m $ parseFromStream (fromJSON <$> json') . responseBody
  return $ case result of
    Left e -> 
      case fromException e of
        Just (StatusCodeException (Status num _) headers _) -> do
              let res = lookup "X-Response-Body-Start" headers 
                  errorJson = case res of 
                    Nothing -> DynamoErrorDetails ClientParsingError "no json body"
                    Just x -> case eitherDecodeStrict x of
                                Left m -> DynamoErrorDetails ClientParsingError (pack m)
                                Right k -> k
              case num of
                code | code >= 400 && code < 500 -> Left $ ClientError code errorJson
                     | code >= 500 -> Left $ ServerError code errorJson
    Right (Success resp) -> Right resp
    Right (Error str)    -> Left $ ParseError str
  where
    createRequest
      :: ByteString
      -> Operation
      -> IO (Either String RequestHeaders)
    createRequest payload operation = do
      (public, private) <- getKeys
      creds <- newCredentials public private
      now   <- getCurrentTime
      signPostRequestIO creds UsEast1 ServiceNamespaceDynamodb now "POST"
           ([] :: UriPath)
           ([] :: UriQuery)
           ( [ ("Accept-Encoding", "gzip")
           ,   ("connection", "Keep-Alive")
           ,   ("content-type", "application/x-amz-json-1.0")
           ,   ("Host", "dynamodb.us-east-1.amazonaws.com:443")
           ,   ("x-amz-target", "DynamoDB_20120810." <> operation)
             ] :: RequestHeaders)
           payload





