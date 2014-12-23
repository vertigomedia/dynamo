{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.AWS.DynamoDB.Abstract
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- Description : This code belongs to Oz
------------------------------------------------------------------------------
module Web.AWS.DynamoDB.Abstract where

import           Control.Monad
import qualified Data.Attoparsec.Text as Atto
import           Data.Aeson
import           Data.Scientific
import qualified Data.Serialize as Ser
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Map as M

import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Int
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import           Data.Time.Calendar
import           Data.Typeable
import           Data.Time.Clock
import           Data.Word

import           Web.AWS.DynamoDB.Util

type Item = M.Map Text DValue

-------------------------------------------------------------------------------
-- | Numeric values stored in DynamoDb. Only used in defining new
-- 'DynVal' instances.
newtype DynNumber = DynNumber { unDynNumber :: Scientific }
    deriving (Eq,Show,Read,Ord,Typeable)

-------------------------------------------------------------------------------
-- | String values stored in DynamoDb. Only used in defining new
-- 'DynVal' instances.
newtype DynString = DynString { unDynString :: T.Text }
    deriving (Eq,Show,Read,Ord,Typeable)

-------------------------------------------------------------------------------
-- | Binary values stored in DynamoDb. Only used in defining new
-- 'DynVal' instances.
newtype DynBinary = DynBinary { unDynBinary :: B.ByteString }
    deriving (Eq,Show,Read,Ord,Typeable)

-------------------------------------------------------------------------------
-- | An internally used closed typeclass for values that have direct
-- DynamoDb representations. Based on AWS API, this is basically
-- numbers, strings and binary blobs.
--
-- This is here so that any 'DynVal' haskell value can automatically
-- be lifted to a list or a 'Set' without any instance code
-- duplication.
--
-- Do not try to create your own instances.
class Ord a => DynData a where
    fromData :: a -> DValue
    toData :: DValue -> Maybe a

instance DynData DynNumber where
    fromData (DynNumber i) = DNum i
    toData (DNum i) = Just $ DynNumber i
    toData _ = Nothing

instance DynData (S.Set DynNumber) where
    fromData set = DNumSet (S.map unDynNumber set)
    toData (DNumSet i) = Just $ S.map DynNumber i
    toData _ = Nothing

instance DynData DynString where
    fromData (DynString i) = DString i
    toData (DString i) = Just $ DynString i
    toData _ = Nothing

instance DynData (S.Set DynString) where
    fromData set = DStringSet (S.map unDynString set)
    toData (DStringSet i) = Just $ S.map DynString i
    toData _ = Nothing

instance DynData DynBinary where
    fromData (DynBinary i) = DBinary i
    toData (DBinary i) = Just $ DynBinary i
    toData _ = Nothing

instance DynData (S.Set DynBinary) where
    fromData set = DBinSet (S.map unDynBinary set)
    toData (DBinSet i) = Just $ S.map DynBinary i
    toData _ = Nothing

instance DynData DValue where
    fromData = id
    toData = Just


-------------------------------------------------------------------------------
-- | Class of Haskell types that can be represented as DynamoDb values.
--
-- This is the conversion layer; instantiate this class for your own
-- types and then use the 'toValue' and 'fromValue' combinators to
-- convert in application code.
--
-- Each Haskell type instantiated with this class will map to a
-- DynamoDb-supported type that most naturally represents it.
class DynData (DynRep a) => DynVal a where

    -- | Which of the 'DynData' instances does this data type directly
    -- map to?
    type DynRep a

    -- | Convert to representation
    toRep :: a -> DynRep a

    -- | Convert from representation
    fromRep :: DynRep a -> Maybe a

-------------------------------------------------------------------------------
-- | Any singular 'DynVal' can be upgraded to a list.
instance (DynData (DynRep [a]), DynVal a) => DynVal [a] where
    type DynRep [a] = S.Set (DynRep a)
    fromRep set = mapM fromRep $ S.toList set
    toRep as = S.fromList $ map toRep as

-------------------------------------------------------------------------------
-- | Any singular 'DynVal' can be upgraded to a 'Set'.
instance (DynData (DynRep (S.Set a)), DynVal a, Ord a) => DynVal (S.Set a) where
    type DynRep (S.Set a) = S.Set (DynRep a)
    fromRep set = fmap S.fromList . mapM fromRep $ S.toList set
    toRep as = S.map toRep as


instance DynVal DValue where
    type DynRep DValue = DValue
    fromRep = Just
    toRep   = id


instance DynVal Int where
    type DynRep Int = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Int8 where
    type DynRep Int8 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Int16 where
    type DynRep Int16 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Int32 where
    type DynRep Int32 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Int64 where
    type DynRep Int64 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Word8 where
    type DynRep Word8 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Word16 where
    type DynRep Word16 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Word32 where
    type DynRep Word32 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Word64 where
    type DynRep Word64 = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal Integer where
    type DynRep Integer = DynNumber
    fromRep (DynNumber i) = toIntegral i
    toRep i = DynNumber (fromIntegral i)


instance DynVal T.Text where
    type DynRep T.Text = DynString
    fromRep (DynString i) = Just i
    toRep i = DynString i


instance DynVal B.ByteString where
    type DynRep B.ByteString = DynBinary
    fromRep (DynBinary i) = Just i
    toRep i = DynBinary i


instance DynVal Double where
    type DynRep Double = DynNumber
    fromRep (DynNumber i) = Just $ toRealFloat i
    toRep i = DynNumber (fromFloatDigits i)


-------------------------------------------------------------------------------
-- | Encoded as number of days
instance DynVal Day where
    type DynRep Day = DynNumber
    fromRep (DynNumber i) = ModifiedJulianDay <$> (toIntegral i)
    toRep (ModifiedJulianDay i) = DynNumber (fromIntegral i)


-------------------------------------------------------------------------------
-- | Losslessly encoded via 'Integer' picoseconds
instance DynVal UTCTime where
    type DynRep UTCTime = DynNumber
    fromRep num = fromTS <$> fromRep num
    toRep x = toRep (toTS x)


-------------------------------------------------------------------------------
pico :: Rational
pico = toRational $ 10 ^ (12 :: Integer)


-------------------------------------------------------------------------------
dayPico :: Integer
dayPico = 86400 * round pico


-------------------------------------------------------------------------------
-- | Convert UTCTime to picoseconds
--
-- TODO: Optimize performance?
toTS :: UTCTime -> Integer
toTS (UTCTime (ModifiedJulianDay i) diff) = i' + diff'
    where
      diff' = floor (toRational diff * pico)
      i' = i * dayPico


-------------------------------------------------------------------------------
-- | Convert picoseconds to UTCTime
--
-- TODO: Optimize performance?
fromTS :: Integer -> UTCTime
fromTS i = UTCTime (ModifiedJulianDay days) diff
    where
      (days, secs) = i `divMod` dayPico
      diff = fromRational ((toRational secs) / pico)


-- | Encoded as 0 and 1.
instance DynVal Bool where
    type DynRep Bool = DynNumber
    fromRep (DynNumber i) = do
        (i' :: Int) <- toIntegral i
        case i' of
          0 -> return False
          1 -> return True
          _ -> Nothing
    toRep b = DynNumber (if b then 1 else 0)



-- | Type wrapper for binary data to be written to DynamoDB. Wrap any
-- 'Serialize' instance in there and 'DynVal' will know how to
-- automatically handle conversions in binary form.
newtype Bin a = Bin { getBin :: a }
    deriving (Eq,Show,Read,Ord,Typeable,Enum)


instance (Ser.Serialize a) => DynVal (Bin a) where
    type DynRep (Bin a) = DynBinary
    toRep (Bin i) = DynBinary (Ser.encode i)
    fromRep (DynBinary i) = either (const Nothing) (Just . Bin) $
                            Ser.decode i

-------------------------------------------------------------------------------
-- | Encode a Haskell value.
toValue :: DynVal a  => a -> DValue
toValue a = fromData $ toRep a


-------------------------------------------------------------------------------
-- | Decode a Haskell value.
fromValue :: DynVal a => DValue -> Maybe a
fromValue d = toData d >>= fromRep


toIntegral :: (Integral a, RealFrac a1) => a1 -> Maybe a
toIntegral sc = Just $ floor sc


-- | Value types natively recognized by DynamoDb. We pretty much
-- exactly reflect the AWS API onto Haskell types.
data DValue
        = DNum Scientific
        | DString T.Text
        | DBinary B.ByteString
        -- ^ Binary data will automatically be
        -- base64 marshalled.
        | DNumSet (S.Set Scientific)
        | DStringSet (S.Set T.Text)
        | DBinSet (S.Set B.ByteString)
          -- ^ Binary data will
          -- automatically be base64
          -- marshalled.
          deriving (Eq,Show,Read,Ord,Typeable)

instance ToJSON DValue where
    toJSON (DNum i)    = object ["N" .= toText i ]
    toJSON (DString i) = object ["S" .= i]
    toJSON (DBinary i) = object ["B" .= (T.decodeUtf8 $ Base64.encode i)]
    toJSON (DNumSet i) = object ["NS" .= map toText (S.toList i)]
    toJSON (DStringSet i) = object ["SS" .= S.toList i]
    toJSON (DBinSet i) = object ["BS" .= map (T.decodeUtf8 . Base64.encode) (S.toList i)]

instance FromJSON DValue where
    parseJSON o = do
      (obj :: [(T.Text, Value)]) <- M.toList `liftM` parseJSON o
      case obj of
        [("N", numStr)] -> DNum <$> parseScientific numStr
        [("S", str)] -> DString <$> parseJSON str
        [("B", bin)] -> do
            res <- (Base64.decode . T.encodeUtf8) <$> parseJSON bin
            either fail (return . DBinary) res
        [("NS", s)] -> do xs <- mapM parseScientific =<< parseJSON s
                          return $ DNumSet $ S.fromList xs
        [("SS", s)] -> DStringSet <$> parseJSON s
        [("BS", s)] -> do
            xs <- mapM (either fail return . Base64.decode . T.encodeUtf8)
                  =<< parseJSON s
            return $ DBinSet $ S.fromList xs

        x -> fail $ "aws: unknown dynamodb value: " ++ show x

      where
        parseScientific (String str) =
            case Atto.parseOnly Atto.scientific str of
              Left e -> fail ("parseScientific failed: " ++ e)
              Right a -> return a
        parseScientific (Number n) = return n
        parseScientific _ = fail "Unexpected JSON type in parseScientific"





