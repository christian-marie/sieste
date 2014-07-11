--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sieste.Raw where

import Control.Monad.Identity
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy.Builder (stringUtf8)
import Data.String
import Pipes
import qualified Pipes.Prelude as Pipes
import Sieste.Classes
import Sieste.Types.SimplePoint
import Sieste.Util
import Snap.Core
import Marquise.Client

raw :: Snap ()
raw = do
    address <- getParam "address"  >>= (\o -> case o of
        Just bs -> return . fromString . S.unpack $ bs
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'address'")

    end <- getParam "end"
        >>= validateW64 (> 0) "end must be > 0" timeNow

    start <- getParam "start"
          >>= validateW64 (< end) "start must be < end" (return $ end - 86400)

    origin <- getParam "origin" >>= (\o -> case o of
        Just bs -> either (const $ writeError 400 $ stringUtf8 "Invalid origin")
                          return
                          (makeOrigin bs)
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    input <- getParam "test" >>= (\o -> return $ case o of
        Just _  -> hoist (return . runIdentity) (readPoints address start end origin)
        Nothing -> hoist liftIO (readPoints address start end origin))

    makeJSON <- getParam "as_double" >>= (\o -> return $ case o of
        Just _  -> Pipes.map AsDouble >-> jsonEncode
        Nothing -> jsonEncode)

    modifyResponse $ setContentType "application/json"
    writeBS "["
    runEffect $ for (input
                     >-> makeJSON
                     >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"
