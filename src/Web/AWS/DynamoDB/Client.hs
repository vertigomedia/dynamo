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
import           Control.Monad.Trans.State
import           Data.Monoid
import           Data.Bool
import           Data.Time

import           Network.HTTP.Types.Header
import           Aws.SignatureV4
import           Aws.General hiding (parse)

import           Pipes
import           Pipes.HTTP
import qualified Pipes.ByteString as PB 
import           Web.AWS.DynamoDB.Helpers

import           Data.Aeson.Parser (json)
import           Pipes.Attoparsec (parse)

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
  res <- try $ withManager tlsManagerSettings $ \m -> 
                 withHTTP req' m $ \resp -> do
                 runEffect $ responseBody resp >-> PB.stdout
  print res
  case res of
    Left (StatusCodeException _ headers _) -> 
      case lookup "X-Response-Body-Start" headers of
        Nothing -> print "no body?"
        Just x -> print x
    Right body -> print body
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






