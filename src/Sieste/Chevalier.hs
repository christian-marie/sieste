--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Sieste.Chevalier where

import Control.Concurrent
import Control.Exception
import Data.Monoid ((<>))
import Data.ProtocolBuffers hiding (field)
import Data.Serialize
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LazyBuilder
import Sieste.Types.Chevalier
import Snap.Core (urlEncode)
import System.Timeout (timeout)
import System.ZMQ4 hiding (source)
import Marquise.Client
import Data.Locator

-- | Chevalier communication thread, reads SourceQuery requsts from an mvar and
-- replies over the included mvar.
chevalier :: String -> MVar SourceQuery -> IO ()
chevalier chevalier_url query_mvar =
    withContext $ \c -> withSocket c Req $ \s -> do
        connect s chevalier_url
        loop s
  where
    loop s = do
        query <- takeMVar query_mvar

        result <- timeout chevalierTimeout . try $ do
            send s [SendMore] (encodeUtf8 $ sourceOrigin query)
            send s [] $ encodeChevalierRequest $ buildChevalierRequest query
            receive s >>= decodeChevalierResponse

        case result of
            -- Timeout has triggered, we assume that the chevalier daemon has
            -- deadlocked or restarted so we need a new connection.
            --
            -- The code that is waiting for a response over the response mvar
            -- has a timeout also, which is shorter and has already triggered.
            Nothing -> chevalier chevalier_url query_mvar
            -- Otherwise we have a response, if it's an error, we pass it along
            -- and restart ourselves just in case.
            Just either_result -> do
                putMVar (sourceResponse query) either_result
                case either_result of
                    Left _ -> chevalier chevalier_url query_mvar
                    _      -> loop s


    chevalierTimeout = 60000000 -- 60s

    encodeChevalierRequest = runPut . encodeMessage

    decodeChevalierResponse bs = do
        let decoded = runGet decodeMessage bs
        parsed <- either (throwIO . BurstDecodeFailure) return decoded
        let chevalier_error = getField $ chevalierError parsed
        case chevalier_error of
            Just e -> throwIO $ ChevalierFailure e
            Nothing -> return $ buildSources parsed

    buildSources r = map urlSafeSource (getField $ sources r)

    urlSafeSource s =
        let ts      = getField $ tags s
            builder = addr <> foldl f "" ts
        in removeTailComma $ LazyBuilder.toLazyText builder
      where
        f acc (SourceTag k v) = acc
            <> LazyBuilder.fromText (urlEncodeText $ getField k)
            <> "~"
            <> LazyBuilder.fromText (urlEncodeText $ getField v)
            <> ","

        addr = 
            let a   = getField $ Sieste.Types.Chevalier.address s
                ad  = pack . show . Address $ fromIntegral a
            in LazyBuilder.fromText (urlEncodeText "address" <> "~" <> ad <> "," )

        urlEncodeText = decodeUtf8 . urlEncode . encodeUtf8 -- fail
        removeTailComma txt
            | LT.null txt = txt
            | otherwise   = LT.init txt

    buildChevalierRequest (SourceQuery q address page page_size _ _ ) = do
        let addr =  case address of
              "*"  -> Nothing
              a -> Just $ fromIntegral $ fromBase62 $ unpack a
        SourceRequest
                { requestTags    = putField $ buildTags q
                , startPage      = putField $ Just $ fromIntegral page
                , sourcesPerPage = putField $ Just $ fromIntegral page_size
                , addressKey     = putField addr
                }

    buildTags q = [ SourceTag { field = putField "*", value = putField q } ]
