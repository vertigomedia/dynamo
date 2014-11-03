{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.AWS.DynamoDB.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.AWS.DynamoDB.Client where

import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Time

import           Network.HTTP.Types.Header
import           Aws.SignatureV4
import           Aws.General
import           Data.Monoid

import           Pipes
import           Pipes.HTTP
import qualified Pipes.ByteString as PB 
import           Web.AWS.DynamoDB.Helpers

type Operation = ByteString
  
callDynamo :: ToJSON a => Operation -> a -> IO ()
callDynamo op bs = do
  req <- parseUrl "http://localhost:8000"--"https://dynamodb.us-east-1.amazonaws.com"    
  let rawjson = L.toStrict . encode $ bs
  print rawjson
  Right heads <- createRequest rawjson op
  let req' = req {
       method = "POST"
     , requestHeaders = heads
     , requestBody = stream $ PB.fromLazy (encode bs)
     }
  withManager tlsManagerSettings $ \m ->
    withHTTP req' m $ \resp -> do
      runEffect $ responseBody resp >-> PB.stdout
  where
    createRequest :: ByteString -> Operation -> IO (Either String RequestHeaders)
    createRequest payload operation = do
     (public, private) <- getKeys
     creds <- newCredentials public private
     now   <- getCurrentTime
     signPostRequestIO creds UsEast1 ServiceNamespaceDynamodb now "POST"
           ([] :: UriPath)
           ([] :: UriQuery)
           ([ ("host", "dynamodb.us-east-1.amazonaws.com")
            , ("x-amz-target", "DynamoDB_20120810." <> operation)
            , ("connection", "Keep-Alive")
            , ("content-type", "application/x-amz-json-1.0")
            ] :: RequestHeaders)
           payload 
  





