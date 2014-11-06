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

import           Control.Applicative
import           Control.Exception
import           Data.Aeson
import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Control.Monad.Trans.State.Strict
import           Data.Monoid
import           Data.Bool
import           Data.Time

import           Network.HTTP.Types.Header
import           Aws.SignatureV4
import           Aws.General hiding (parse)

import           Pipes.HTTP
import qualified Pipes.ByteString as PB 
import           Web.AWS.DynamoDB.Helpers
import           Pipes.Attoparsec (parse, ParsingError(..))

type Operation = ByteString

dev :: Bool
dev = False

data DynamoError = ProducerExhausted | ParseError

callDynamo ::
  (ToJSON a) =>
  Operation -> a -> IO (Maybe (Either ParsingError Value))
callDynamo op bs = do
  let url = bool "https://dynamodb.us-east-1.amazonaws.com:443" "http://localhost:8000" dev
  req <- parseUrl url
  let rawjson = L.toStrict $ encode bs
  print rawjson
  Right heads <- createRequest rawjson op
  let req' = req {
       method = "POST"
     , requestHeaders = heads
     , requestBody = stream $ PB.fromLazy (encode bs)
    }
  print req'
  res <- withManager tlsManagerSettings $ \m -> 
           withHTTP req' m $ \resp -> do
             evalStateT (parse json') (responseBody resp)
                   -- case result of
                   --   Left (ParsingError x) -> return $ Left x
                   --   Right val -> Right val
  return res
  -- return $ case res of
  --   Left (StatusCodeException _ headers _) -> 
  --     case lookup "X-Response-Body-Start" headers of
  --       Nothing -> Left "no body?"
  --       Just x -> Left "oops"
  --   Right body -> Right body
  where
    createRequest :: ByteString -> Operation -> IO (Either String RequestHeaders)
    createRequest payload operation = do
      (public, private) <- getKeys
      creds <- newCredentials public private
      now   <- getCurrentTime
      signPostRequestIO creds UsEast1 ServiceNamespaceDynamodb now "POST"
           ([] :: UriPath)
           ([] :: UriQuery)
             ( [("Accept-Encoding", "gzip")
             , ("connection", "Keep-Alive")
             , ("content-type", "application/x-amz-json-1.0")
             , ("Host", "dynamodb.us-east-1.amazonaws.com:443")
             , ("x-amz-target", "DynamoDB_20120810." <> operation)
             ] :: RequestHeaders)
             payload





