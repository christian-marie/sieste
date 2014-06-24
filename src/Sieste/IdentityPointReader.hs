--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others 
--
-- The code in this file, and the program it is a part of, is 	  				 	 	 	   	 	  	   		  	 
-- made available to you by its authors as open source software: 	  				 	 	 	   	 	  	   		  	 
-- you can redistribute it and/or modify it under the terms of 	  				 	 	 	   	 	  	   		  	 
-- the 3-clause BSD licence. 
--

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE InstanceSigs #-}

module Sieste.IdentityPointReader
(
) where

import Sieste.Classes
import Control.Monad.Identity
import Marquise.Client
import Data.Word(Word64)
import Pipes
import Data.ReinterpretCast

instance PointReader Identity where
    readPoints :: Address -> Word64 -> Word64 -> Origin -> Producer SimplePoint Identity ()

    -- Are the times in this supposed to go through toEpoch? If so, they then typecheck as SimplePoint expects Word64, not Int

    -- Address for Ints
    readPoints 0 start end _ = do
        yield (SimplePoint 0 start 0)
        let mid = (start + end) `div` 2
        yield (SimplePoint 0 mid 1)
        yield (SimplePoint 0 end 2)

    -- Address for Doubles
    readPoints 1 start end _ = do
        yield (SimplePoint 0 start (doubleToWord 0.0))
        let mid = (start + end) `div` 2
        yield (SimplePoint 0 mid (doubleToWord 1.1))
        yield (SimplePoint 0 end (doubleToWord 2.2))

    readPoints _ _ _ _ = error "invalid address, expect 0 (for ints) or 1 (for doubles)"
