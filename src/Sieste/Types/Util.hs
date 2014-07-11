--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Sieste.Types.Util where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Text.Lazy as LT

data PrettyError = PrettyError { message :: LT.Text }
instance ToJSON PrettyError where
    toJSON (PrettyError m) = object [ "error" .= m ]
