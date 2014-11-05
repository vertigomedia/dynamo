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

import           Control.Exception
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Monoid
import           Data.Bool
import           Data.Time

import           Network.HTTP.Types.Header
import           Aws.SignatureV4
import           Aws.General

import           Pipes
import           Pipes.HTTP
import qualified Pipes.ByteString as PB 
import           Web.AWS.DynamoDB.Helpers

type Operation = ByteString

dev :: Bool
dev = True

callDynamo :: ToJSON a => Operation -> a -> IO ()
callDynamo op bs = do
  let url = bool "https://dynamodb.us-east-1.amazonaws.com" "http://localhost:8000" dev
  req <- parseUrl url
  let rawjson = L.toStrict $ encode bs
  --print rawjson
  Right heads <- createRequest rawjson op
  let req' = req {
       method = "POST"
     , requestHeaders = heads
     , requestBody = stream $ PB.fromLazy (encode bs)
    }
  withManager tlsManagerSettings $ \m -> do
     res <- try (withHTTP req' m $ \resp -> do
              runEffect $ responseBody resp >-> PB.stdout) :: IO (Either HttpException ()) 
     print res
     case res of
       Left (StatusCodeException _ headers _) -> 
         do print "caught"
       _ -> print "all good"
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






