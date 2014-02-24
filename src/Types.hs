{-# LANGUAGE DeriveGeneric #-}
module Types where

import           Control.Concurrent
import           Data.ByteString      (ByteString)
import           Data.Int             (Int64)
import           Data.ProtocolBuffers hiding (field)
import           Data.TypeLevel       (D1, D2, D3)
import           GHC.Generics         (Generic)

data ChevalierTag = ChevalierTag
    { field :: Required D1 (Value ByteString)
    , value :: Required D2 (Value ByteString)
    } deriving (Generic, Eq, Show)
instance Encode ChevalierTag

data ChevalierRequest = ChevalierRequest
    { tags           :: Repeated D1 (Message ChevalierTag)
    , startPage      :: Optional D2 (Value Int64)
    , sourcesPerPage :: Optional D3 (Value Int64)
    } deriving (Generic, Eq, Show)
instance Encode ChevalierRequest

data SourceQuery = SourceQuery
    { queryRequest  :: ByteString
    , queryPage     :: Integer
    , queryResponse :: MVar ByteString }
