{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

module Descartes.Types.ReaderD where

import           Control.Exception
import qualified Data.Aeson             as A
import qualified Data.Attoparsec.Number as AN
import           Data.ByteString        (ByteString)
import           Data.ByteString.Base64 as B64
import           Data.Int               (Int64)
import           Data.List              (intercalate)
import           Data.Maybe
import           Data.ProtocolBuffers   hiding (field)
import           Data.Text              (Text, unpack)
import           Data.Text.Encoding     (decodeUtf8)
import           Data.Typeable
import qualified Data.Vector            as V

import           Data.TypeLevel         (D1, D2, D3, D4, D5, D6, D7, D8)
import           Data.Word              (Word64)
import           GHC.Generics           (Generic)
import           Pipes.Concurrent

-- A request to the reader daemon thread to retrieve a stream of chunks from
-- given range. The chunks are ordered in so far as the earliest point in a
-- later chunk is no earlier than the latest point in the preceding chunk.
data RangeQuery = RangeQuery
    { rangeSource :: [SourceTag]
    , rangeStart  :: Word64
    , rangeEnd    :: Word64
    , rangeOrigin :: Text
    , rangeOutput :: Output (Either SomeException DataBurst)
    }

-- Custom exceptions

data ReaderDException = BurstDecodeFailure String
                      | ZMQTimeout
                      | DecompressionFailure
      deriving (Show, Typeable)
instance Exception ReaderDException

-- Protobufs follow

data SourceTag = SourceTag
    { field :: Required D1 (Value Text)
    , value :: Required D2 (Value Text)
    } deriving (Generic, Eq)
instance Encode SourceTag
instance Decode SourceTag

instance Show SourceTag where
        show t =
            unpack (getField $ field t) ++
            ": " ++
            unpack (getField $ value t)

instance Show [SourceTag] where
    show tags = intercalate ", " $ map show tags

data DataFrame = DataFrame
    { origin           :: Optional D8 (Value Text),
      source           :: Repeated D1 (Message SourceTag),
      timestamp        :: Required D2 (Value (Fixed Word64)),
      payload          :: Required D3 (Enumeration ValueType),
      valueNumeric     :: Optional D4 (Value Int64),
      valueMeasurement :: Optional D5 (Value Double),
      valueTextual     :: Optional D6 (Value Text),
      valueBlob        :: Optional D7 (Value ByteString)
    } deriving (Generic, Eq, Show)

instance Decode DataFrame
instance A.ToJSON DataFrame where
    toJSON DataFrame{..}
        | getField payload == EMPTY = A.Array $ V.fromList
            [ "binary"
            , getEpoch
            , A.Null ]
        | getField payload == NUMBER = A.Array $ V.fromList
            [ "number"
            , getEpoch
            , A.Number $ AN.I $ fromIntegral $ fromJust $ getField valueNumeric]
        | getField payload == REAL = A.Array $ V.fromList
            [ "real"
            , getEpoch
            , A.Number $ AN.D $ fromJust $ getField valueMeasurement]
        | getField payload == TEXT = A.Array $ V.fromList
            [ "text"
            , getEpoch
            , A.String $ fromJust $ getField valueTextual]
        | getField payload == BINARY = A.Array $ V.fromList
            [ "binary"
            , getEpoch
            , A.String $ decodeUtf8 $ B64.encode (fromJust $ getField valueBlob)]
        | otherwise = error "Unrepresentable DataFrame type"
      where
        getEpoch = A.Number $ AN.I $ fromIntegral (getField timestamp) `div` 1000000000

data DataBurst = DataBurst
    { frames :: Repeated D1 (Message DataFrame)
    } deriving (Generic, Eq, Show)

instance Decode DataBurst

data ValueType = EMPTY
               | NUMBER
               | REAL
               | TEXT
               | BINARY
      deriving (Enum, Generic, Show, Eq)

instance Decode ValueType

data RequestMulti = RequestMulti
    { multiRequestsField :: Repeated D1 (Message RequestSource)
    } deriving (Generic, Eq, Show)
instance Encode RequestMulti

data RequestSource = RequestSource
    {
      requestSourceField :: Repeated D1 (Message SourceTag),
      requestAlphaField  :: Required D2 (Value (Fixed Word64)),
      requestOmegaField  :: Optional D3 (Value (Fixed Word64))
    } deriving (Generic, Eq, Show)

instance Encode RequestSource
