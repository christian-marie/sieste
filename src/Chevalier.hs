{-# LANGUAGE OverloadedStrings #-}
module Chevalier where

import           Control.Concurrent
import           Control.Exception
import           Data.Monoid            ((<>))
import           Data.ProtocolBuffers   hiding (field)
import           Data.Serialize
import qualified Data.Text.Lazy.Builder as LazyBuilder
import           System.Timeout         (timeout)
import           System.ZMQ4            hiding (source)
import           Types.Chevalier

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
            -- and restart ourselves just incase
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
            builder = foldl f "" ts
        in LazyBuilder.toLazyText builder
      where
        f acc (SourceTag k v) = acc
            <> (LazyBuilder.fromText $ getField v)
            <> "~"
            <> (LazyBuilder.fromText $ getField k)
            <> ","

    buildChevalierRequest (SourceQuery q page _ ) = SourceRequest
        { requestTags    = putField $ buildTags q
        , startPage      = putField $ Just $ fromIntegral page
        , sourcesPerPage = putField $ Just 64
        }

    buildTags q = [ SourceTag { field = putField "*", value = putField q } ]
