{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Types.Chevalier where

import           Control.Concurrent
import           Control.Exception
import           Data.Int             (Int64)
import           Data.ProtocolBuffers hiding (field)
import           Data.Text            (Text)
import qualified Data.Text.Lazy       as LT
import           Data.Typeable
import           Data.TypeLevel       (D1, D2, D3)
import           GHC.Generics         (Generic)

-- A request to the chevalier thread to search for a source by a single keyword
data SourceQuery = SourceQuery
    { sourceRequest  :: Text
    , sourcePage     :: Integer
    , sourceResponse :: MVar (Either SomeException [LT.Text]) }

-- Custom Exceptions
data ChevalierException = BurstDecodeFailure String
                        | ChevalierFailure Text
    deriving (Show, Typeable)
instance Exception ChevalierException


-- Protobufs follow

data SourceTag = SourceTag
    { field :: Required D1 (Value Text)
    , value :: Required D2 (Value Text)
    } deriving (Generic, Eq, Show)
instance Encode SourceTag
instance Decode SourceTag

data SourceRequest = SourceRequest
    { requestTags    :: Repeated D1 (Message SourceTag)
    , startPage      :: Optional D2 (Value Int64)
    , sourcesPerPage :: Optional D3 (Value Int64)
    } deriving (Generic, Eq, Show)
instance Encode SourceRequest

data SourceResponse = ChevalierResponse
    { sources        :: Repeated D1 (Message Source)
    , chevalierError :: Optional D2 (Value Text)
    } deriving (Generic, Eq, Show)
instance Decode SourceResponse

data Source = Source
    { tags :: Repeated D1 (Message SourceTag)
    } deriving (Generic, Eq, Show)
instance Decode Source
