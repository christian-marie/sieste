--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

module Sieste.Types.SimplePoint where

import qualified Data.Aeson as A
import Data.ReinterpretCast
import qualified Data.Vector as V
import Marquise.Types
import Sieste.IdentityPointReader ()
import Sieste.IOPointReader ()

newtype AsDouble = AsDouble SimplePoint

instance A.ToJSON AsDouble where
    toJSON (AsDouble (SimplePoint _ time payload)) =
        A.Array $ V.fromList [ A.toJSON time, A.toJSON (wordToDouble payload) ]

instance A.ToJSON SimplePoint where
    toJSON (SimplePoint _ time payload) = A.Array $ V.fromList [A.toJSON time, A.toJSON payload ]

instance A.ToJSON TimeStamp where
    toJSON (TimeStamp ts) = A.toJSON ts
