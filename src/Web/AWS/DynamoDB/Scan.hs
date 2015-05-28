{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Aws.DynamoDb.Commands.Scan
-- Copyright   :  Soostone Inc
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- Stability   :  experimental
--
-- Implementation of Amazon DynamoDb Scan command.
--
-- See: @http:\/\/docs.aws.amazon.com\/amazondynamodb\/latest\/APIReference\/API_Scan.html@
----------------------------------------------------------------------------

module Web.AWS.DynamoDB.Scan
    ( Scan (..)
    , scan
    , ScanResponse (..)
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Default
import           Data.Maybe
import qualified Data.Text           as T
import           Data.Typeable
import qualified Data.Vector         as V
-------------------------------------------------------------------------------
import           Web.AWS.DynamoDB.Core
import           Web.AWS.DynamoDB.Types
-------------------------------------------------------------------------------


-- | A Scan command that uses primary keys for an expedient scan.
data Scan = Scan {
      sTableName     :: T.Text
    -- ^ Required.
    , sFilter        :: Conditions
    -- ^ Whether to filter results before returning to client
    , sStartKey      :: Maybe [Attribute]
    -- ^ Exclusive start key to resume a previous query.
    , sLimit         :: Maybe Int
    -- ^ Whether to limit result set size
    , sSelect        :: QuerySelect
    -- ^ What to return from 'Scan'
    , sRetCons       :: ReturnConsumption
    , sSegment       :: Int
    -- ^ Segment number, starting at 0, for parallel queries.
    , sTotalSegments :: Int
    -- ^ Total number of parallel segments. 1 means sequential scan.
    } deriving (Eq,Show,Read,Ord,Typeable)


-- | Construct a minimal 'Scan' request.
scan :: T.Text                   -- ^ Table name
     -> Scan
scan tn = Scan tn def Nothing Nothing def def 0 1


-- | Response to a 'Scan' query.
data ScanResponse = ScanResponse {
      srItems    :: V.Vector Item
    , srLastKey  :: Maybe [Attribute]
    , srCount    :: Int
    , srScanned  :: Int
    , srConsumed :: Maybe ConsumedCapacity
    } deriving (Eq,Show,Read,Ord)


-------------------------------------------------------------------------------
instance ToJSON Scan where
    toJSON Scan{..} = object $
      catMaybes
        [ (("ExclusiveStartKey" .= ) . attributesJson) <$> sStartKey
        , ("Limit" .= ) <$> sLimit
        ] ++
      conditionsJson "ScanFilter" sFilter ++
      querySelectJson sSelect ++
      [ "TableName".= sTableName
      , "ReturnConsumedCapacity" .= sRetCons
      , "Segment" .= sSegment
      , "TotalSegments" .= sTotalSegments
      ]


instance FromJSON ScanResponse where
    parseJSON (Object v) = ScanResponse
        <$> v .:?  "Items" .!= V.empty
        <*> ((do o <- v .: "LastEvaluatedKey"
                 Just <$> parseAttributeJson o)
             <|> pure Nothing)
        <*> v .:  "Count"
        <*> v .:  "ScannedCount"
        <*> v .:? "ConsumedCapacity"
    parseJSON _ = fail "ScanResponse must be an object."


------------------------------------------------------------------------------
-- | `DynamoAction` instance
instance DynamoAction Scan ScanResponse

