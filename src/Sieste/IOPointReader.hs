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

import Marquise.Client
import Pipes
import Pipes.Concurrent
import Sieste.Classes
import System.Environment

instance PointReader IO where
    readPoints addr start end origin = do
        (o,i,s) <- liftIO $ spawn' (Bounded 1024)
        _ <- liftIO . forkIO $ do
            broker <- getEnv "BROKER_URL"
            withReaderConnection broker $ \c ->
                runEffect (readSimple addr (TimeStamp start) (TimeStamp end) origin c >-> decodeSimple >-> toOutput o)
            atomically s
        fromInput i
