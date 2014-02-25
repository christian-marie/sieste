{-# LANGUAGE OverloadedStrings #-}
module Chevalier where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Data.Monoid            ((<>))
import           Data.ProtocolBuffers   hiding (field)
import           Data.Serialize
import qualified Data.Text.Lazy.Builder as LazyBuilder
import           System.ZMQ4            hiding (source)
import           Types
import           System.Timeout               (timeout)

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
            response <- decodeChevalierResponse <$> receive s
            either error (return . buildSources) response

        case result of 
            -- Timeout has triggered, we assume that the chevalier daemon has
            -- deadlocked or restarted so we need a new connection.
            --
            -- The code that is waiting for a response over the response mvar
            -- has a timeout also, which is shorter and has already triggered.
            Nothing -> chevalier chevalier_url query_mvar
            -- Otherwise we have a response, error or not.
            Just result' -> putMVar (sourceResponse query) result'


    chevalierTimeout = 60000000 -- 60s

    encodeChevalierRequest = runPut . encodeMessage

    decodeChevalierResponse = runGet decodeMessage

    buildSources r = map urlSafeSource (getField $ sources r)

    urlSafeSource s =
        let tags = getField $ source s
            builder = foldl f "" tags
        in LazyBuilder.toLazyText builder
      where
        f acc (ChevalierTag k v) = acc
            <> (LazyBuilder.fromText $ getField v)
            <> "~"
            <> (LazyBuilder.fromText $ getField k)
            <> ","

    buildChevalierRequest (SourceQuery q page _ ) = ChevalierRequest
        { requestTags    = putField $ buildTags q
        , startPage      = putField $ Just $ fromIntegral page
        , sourcesPerPage = putField $ Just 64
        }

    buildTags q = [ ChevalierTag { field = putField "*", value = putField q } ]
