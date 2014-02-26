{-# LANGUAGE OverloadedStrings  #-}

module Types.Util where

import           Data.Aeson           (ToJSON, object, toJSON, (.=))
import qualified Data.Text.Lazy       as LT

data PrettyError = PrettyError { message :: LT.Text }
instance ToJSON PrettyError where
    toJSON (PrettyError m) = object [ "error" .= m ]
