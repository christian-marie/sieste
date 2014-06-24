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
import Data.Text

instance PointReader Identity where
    readPoints :: Address -> Word64 -> Word64 -> Origin -> Producer SimplePoint Identity ()

    readPoints 0 start end origin = do
        yield (SimplePoint 0 start 0)
        let mid = (start + end) `div` 2
        yield (SimplePoint 0 mid 1)
        yield (SimplePoint 0 end 2)

    readPoints 1 start end origin = do
        yield (SimplePoint 0 start (doubleToWord 0.0))
        let mid = (start + end) `div` 2
        yield (SimplePoint 0 mid (doubleToWord 1.0))
        yield (SimplePoint 0 end (doubleToWord 2.0))

    readPoints _ _ _ _ = error "invalid address, expect 0 or 1"
