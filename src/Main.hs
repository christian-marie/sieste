{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Http.Server
import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Control.Concurrent
import Data.Int (Int64)
import System.Environment(getEnv)
import System.ZMQ4.Monadic hiding (async)

import Data.ProtocolBuffers hiding (field)
import GHC.Generics(Generic)
import Data.TypeLevel(D1, D2, D3)
import Data.Serialize
import Control.Concurrent.Async
import Control.Monad

data ChevalierTag = ChevalierTag
    { field :: Required D1 (Value ByteString)
    , value :: Required D2 (Value ByteString)
    } deriving (Generic, Eq, Show)
instance Encode ChevalierTag

data ChevalierRequest = ChevalierRequest
    { tags           :: Repeated D1 (Message ChevalierTag)
    , startPage      :: Optional D2 (Value Int64)
    , sourcesPerPage :: Optional D3 (Value Int64)
    } deriving (Generic, Eq, Show)
instance Encode ChevalierRequest

data SourceQuery = SourceQuery
    { queryRequest  :: ByteString
    , queryPage     :: Int64
    , queryResponse :: MVar ByteString }

main :: IO ()
main = do
    chevalier_url <- getEnv "CHEVALIER_URL"
    query_mvar <- newEmptyMVar

    asyncs <- replicateM 10 $
        async $ chevalier chevalier_url query_mvar

    async $ watchThreads asyncs (chevalier chevalier_url query_mvar)

    quickHttpServe $
        ifTop (writeBS docString) <|>
        route [ ("simple/search", simpleSearch query_mvar) ]
  where
    watchThreads as restart_action = do
        (action, err) <- waitAnyCatch as

        case err of
            Right _ -> putStrLn "Thread exited normally (shouldn't happen)"
            Left e -> putStrLn $ "Thread exploded: " ++ show e

        let remaining_threads = filter (/= action) as
        a <- async restart_action

        threadDelay 1000000
        watchThreads (a:remaining_threads) restart_action

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
        , startPage = putField $ Just page
        , sourcesPerPage = putField $ Just 64
        }
    buildTags q = putField
        [ ChevalierTag { field = putField "*", value = putField q } ]
                
simpleSearch :: MVar SourceQuery -> Snap ()
simpleSearch query_mvar = do
    query <- fromMaybe "*" <$> getParam "q"
    page <- toInt64 <$> fromMaybe "0" <$> getParam "page"
    response_mvar <- liftIO newEmptyMVar
    liftIO $ putMVar query_mvar $ SourceQuery query page response_mvar
    response <- liftIO $ takeMVar response_mvar
    writeBS response

toInt64 :: ByteString -> Int64
toInt64 bs = fromIntegral $ maybe 0 fst (B.readInteger bs)

linkThread :: IO a -> IO ()
linkThread a = async a >>= link

docString :: ByteString
docString = "This is the Vaultaire REST interface, you can find \
            \documentation in the wiki under TODO"
