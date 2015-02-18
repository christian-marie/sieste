--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Sieste.SimpleSearch where

import Chevalier.Types
import Chevalier.Util
import Control.Applicative
import Control.Arrow
import Control.Concurrent hiding (yield)
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Builder (stringUtf8)
import Data.Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Sieste.Util
import Snap.Core
import System.Timeout (timeout)

simpleSearch :: MVar SourceQuery -> Snap ()
simpleSearch chevalier_mvar = do
    -- Address can be used to get information based on an ID
    -- If address is defined, query strings are ignored.
    address   <- utf8Or400 =<< fromMaybe "*"  <$> getParam "address"
    page      <- toInt     <$> fromMaybe "0"  <$> getParam "page"
    page_size <- toInt     <$> fromMaybe "64" <$> getParam "page_size"

    origin <- getParam "origin" >>= (\o -> case o of
        Just bs -> utf8Or400 bs
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    -- From all the 'q' parameters, build into Tags
    -- if q ~= "key:value" -> ("key", "value")
    --    otherwise        -> ("*"  , "q")
    -- allows for query strings like "?q=hostname:foo&q=*bar*"
    request <- getRequest
    let query = case Data.Map.lookup "q" (rqQueryParams request) of
                Just _  -> [ let s = TE.decodeUtf8 x
                               in  if T.isInfixOf ":" s
                                    then let (k, v) = second T.tail $ T.span (/= ':') s in buildTag k v
                                    else buildWildcardTag s
                              | x <- (rqQueryParams request) ! "q"  ]
                Nothing -> []

    maybe_response <- liftIO $ do
        response_mvar <- newEmptyMVar
        putMVar chevalier_mvar $
            SourceQuery query address page page_size origin response_mvar
        timeout chevalierTimeout $ takeMVar response_mvar

    either_response <- maybe timeoutError return maybe_response
    either chevalierError writeJSON either_response
  where
    chevalierTimeout = 10000000 -- 10 seconds

    chevalierError e =
        writeError 500 $ stringUtf8 ("Exception talking to chevalier backend" ++ show e)

    timeoutError = do
        let msg = "Timed out talking to chevalier backend"
        writeError 500 $ stringUtf8 msg
