{-# LANGUAGE OverloadedStrings #-}
module Chevalier where

import           Control.Concurrent
import           Control.Monad
import           Data.ProtocolBuffers hiding (field)
import           Data.Serialize
import           System.ZMQ4.Monadic  hiding (async)
import           Types

-- | Chevalier communication thread, reads SourceQuery requsts from an mvar and
-- replies over the included mvar.
chevalier :: String -> MVar SourceQuery -> IO ()
chevalier chevalier_url query_mvar =
    runZMQ $ do
        req_socket <- socket Req
        connect req_socket chevalier_url

        forever $ do
            query <- liftIO $ takeMVar query_mvar
            -- Build the protobuf request
            send req_socket [] $ encodeChevalierRequest $ buildChevalierRequest query

            response <- receive req_socket
            liftIO $ putMVar (queryResponse query) response
  where
    encodeChevalierRequest = runPut . encodeMessage
    buildChevalierRequest (SourceQuery q page _ ) = ChevalierRequest
        { tags = buildTags q
        , startPage = putField $ Just $ fromIntegral page
        , sourcesPerPage = putField $ Just 64
        }
    buildTags q = putField
        [ ChevalierTag { field = putField "*", value = putField q } ]
