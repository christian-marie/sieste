--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others 
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence. 
--

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sieste.IOPointReader
 ( readPoints ) 
where

import Sieste.Classes

instance PointReader IO where
    readPoints =  error "Implement PointReader for Vaultaire backend"

--    readPoints address start end origin =
--       withConnection "broker" $  \c -> decodeSimple address start end origin c --(withConnection "broker")
--         --   >-> decodeSimple
