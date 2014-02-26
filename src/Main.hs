{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Chevalier                (chevalier)
import           ReaderD                (readerd)
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Simple                   (simpleSearch, interpolated)
import           Snap.Core
import           Snap.Http.Server
import           System.Environment       (getEnv)

main :: IO ()
main = do
    chevalier_url <- getEnv "CHEVALIER_URL"
    chevalier_query_mvar <- newEmptyMVar
    let start_chevalier = chevalier chevalier_url chevalier_query_mvar

    readerd_url <- getEnv "READERD_URL"
    readerd_query_mvar <- newEmptyMVar
    let start_readerd = readerd readerd_url readerd_query_mvar


    chevalier_threads <- replicateM 16 $ async start_chevalier
    readerd_threads   <- replicateM 16 $ async start_readerd

    async $ watchThreads chevalier_threads start_chevalier
    async $ watchThreads readerd_threads start_readerd

    quickHttpServe $
        ifTop (writeBS docString) <|>
        route [ ("simple/search", simpleSearch chevalier_query_mvar)
              , ("interpolated/:source", interpolated readerd_query_mvar) ]

  where
    -- Wait for any thread to explode, then restart it whilst logging the
    -- exception.
    watchThreads as restart_action = do
        (a, err) <- waitAnyCatch as

        case err of
            Right _ -> putStrLn "Thread exited normally (shouldn't happen)"
            Left e -> putStrLn $ "Thread exploded: " ++ show e

        restarted <- async restart_action
        threadDelay 1000000

        let remaining_threads = filter (/= a) as
        watchThreads (restarted:remaining_threads) restart_action

    docString = "This is the Vaultaire REST interface, you can find \
                \documentation in the wiki under TODO"
