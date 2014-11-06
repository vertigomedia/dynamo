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
import           Data.CaseInsensitive

import           Network.HTTP.Types.Header
import           Aws.SignatureV4
import           Aws.General hiding (parse)
import           Network.Http.Client
import           Web.AWS.DynamoDB.Helpers
import           OpenSSL
import qualified System.IO.Streams as S

type Operation = ByteString

dev :: Bool
dev = False

callDynamo :: (FromJSON b, ToJSON a) => ByteString -> a -> IO (Either String b)
callDynamo op bs = withOpenSSL $ do
  ctx <- baselineContextSSL
  let url = bool "https://dynamodb.us-east-1.amazonaws.com" "http://localhost:8000" dev
  Right heads <- createRequest (L.toStrict $ encode bs) op
  either handleException handleConn =<<
    if dev
      then try (openConnectionSSL ctx "dynamodb.us-east-1.amazonaws.com" 443) :: IO (Either SomeException Connection)
      else try (openConnection "localhost" 8000) :: IO (Either SomeException Connection) 
  where
    handleException _ = undefined
    handleConn conn = do
       let jsonBody = L.toStrict $ encode bs
       result <- createRequest op jsonBody
       case result of
         Left msg -> return $ Left $ "Couldn't create headers: " <> msg
         Right headers -> do
           stream <- S.fromByteString jsonBody
           req <- buildRequest $ do
              http POST "/"
              mapM_ makeHeader headers
           print req
           sendRequest conn req $ inputStreamBody stream
           json <- receiveResponse conn jsonHandler
           closeConnection conn
           return (Right json)
    makeHeader :: Header -> RequestBuilder ()
    makeHeader (key, val) = setHeader (original key) val
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






