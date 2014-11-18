--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

module Sieste.Classes
(
    PointReader(..)
) where

import Data.Word (Word64)
import Marquise.Client
import Pipes

class Monad m => PointReader m where
    readPoints :: Address -> Word64 -> Word64 -> Origin
               -> Producer SimplePoint m ()
