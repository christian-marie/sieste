{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Concurrent
import           Control.Exception
import           Data.Aeson           (ToJSON, object, toJSON, (.=))
import           Data.Int             (Int64)
import           Data.ProtocolBuffers hiding (field)
import           Data.Text            (Text)
import qualified Data.Text.Lazy       as LT
import           Data.TypeLevel       (D1, D2, D3)
import           GHC.Generics         (Generic)

data ChevalierTag = ChevalierTag
    { field :: Required D1 (Value Text)
    , value :: Required D2 (Value Text)
    } deriving (Generic, Eq, Show)
instance Encode ChevalierTag
instance Decode ChevalierTag

data ChevalierRequest = ChevalierRequest
    { requestTags    :: Repeated D1 (Message ChevalierTag)
    , startPage      :: Optional D2 (Value Int64)
    , sourcesPerPage :: Optional D3 (Value Int64)
    } deriving (Generic, Eq, Show)
instance Encode ChevalierRequest

data ChevalierResponse = ChevalierResponse
    { sources :: Repeated D1 (Message ChevalierSource)
    } deriving (Generic, Eq, Show)
instance Decode ChevalierResponse

data ChevalierSource = ChevalierSource
    { source :: Repeated D1 (Message ChevalierTag)
    } deriving (Generic, Eq, Show)
instance Decode ChevalierSource

data SourceQuery = SourceQuery
    { queryRequest  :: Text
    , queryPage     :: Integer
    , queryResponse :: MVar (Either SomeException [LT.Text]) }

data PrettyError = PrettyError { message :: LT.Text }
instance ToJSON PrettyError where
    toJSON (PrettyError m) = object [ "message" .= m ]
