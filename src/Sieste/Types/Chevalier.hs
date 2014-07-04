{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Sieste.Types.Chevalier where

import Control.Concurrent
import Control.Exception
import Data.Int (Int64)
import Data.ProtocolBuffers hiding (field)
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Data.Typeable
import Data.Word (Word64)
import GHC.Generics (Generic)

-- A request to the chevalier thread to search for a source by a single keyword
data SourceQuery = SourceQuery
    { sourceRequest  :: Text
    , sourcePage     :: Integer
    , pageSize       :: Integer
    , sourceOrigin   :: Text
    , sourceResponse :: MVar (Either SomeException [LT.Text]) }

-- Custom Exceptions
data ChevalierException = BurstDecodeFailure String
                        | ChevalierFailure Text
    deriving (Show, Typeable)
instance Exception ChevalierException

-- Protobufs follow

data SourceTag = SourceTag
    { field :: Required 1 (Value Text)
    , value :: Required 2 (Value Text)
    } deriving (Generic, Eq, Show)
instance Encode SourceTag
instance Decode SourceTag

data SourceRequest = SourceRequest
    { requestTags    :: Repeated 1 (Message SourceTag)
    , startPage      :: Optional 2 (Value Int64)
    , sourcesPerPage :: Optional 3 (Value Int64)
    } deriving (Generic, Eq, Show)
instance Encode SourceRequest

data SourceResponse = ChevalierResponse
    { sources        :: Repeated 1 (Message Source)
    , chevalierError :: Optional 2 (Value Text)
    } deriving (Generic, Eq, Show)
instance Decode SourceResponse

data Source = Source
    { tags    :: Repeated 1 (Message SourceTag)
    , address :: Required 3 (Value (Fixed Word64))
    } deriving (Generic, Eq, Show)
instance Decode Source
