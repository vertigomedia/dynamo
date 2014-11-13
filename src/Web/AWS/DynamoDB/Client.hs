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
import           Data.Text    (pack)
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
import           Pipes.Attoparsec (parse) -- ParsingError(..)

import           Web.AWS.DynamoDB.Types

------------------------------------------------------------------------------
-- | Operation Type
type Operation = ByteString

------------------------------------------------------------------------------
-- | Development Mode flag
dev :: Bool
dev = True

------------------------------------------------------------------------------
-- | Request issuer
callDynamo
  :: (ToJSON a, FromJSON b)
  => Operation
  -> a
  -> IO (Either DynamoError b)
callDynamo op bs = do
  let url = bool "https://dynamodb.us-east-1.amazonaws.com:443" "http://localhost:8000" dev
  req <- parseUrl url
  let rawjson = L.toStrict $ encode bs
  Right heads <- createRequest rawjson op
  let req' = req {
       method = "POST"
     , requestHeaders = heads
     , requestBody = stream $ PB.fromLazy (encode bs)
    }
  res <- try (withManager tlsManagerSettings $ \m -> 
           withHTTP req' m $ \resp -> do
             evalStateT (parse $ fromJSON <$> json') (responseBody resp))
  case res of
    Left e -> 
      case fromException e of
        Just (StatusCodeException (Status num _) headers _) -> do
--              print $ lookup "X-Response-Body-Start" headers 
              let res = lookup "X-Response-Body-Start" headers 
                  errorJson = case res of 
                    Nothing -> DynamoErrorDetails ClientParsingError "no json body"
                    Just x -> case eitherDecodeStrict x of
                                Left m -> DynamoErrorDetails ClientParsingError (pack m)
                                Right k -> k
              case num of
                code | code >= 400 && code < 500 -> do
                         return $ Left $ ClientError code errorJson
                     | code >= 500 -> do
                         return $ Left $ ServerError code errorJson
        _ -> return $ Left $ UnknownError (show e)
    Right resp -> do
     case resp of
       Nothing -> return $ Left ParseError
       Just x ->  
         case x of
           Left _ -> return $ Left ParseError
           Right y -> 
             case y of
               Success z -> return $ Right z
               Error g   -> return $ Left $ Err g
  where
    createRequest :: ByteString -> Operation -> IO (Either String RequestHeaders)
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





