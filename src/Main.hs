{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Chevalier                (chevalier)
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Simple                   (simpleSearch)
import           Snap.Core
import           Snap.Http.Server
import           System.Environment       (getEnv)

main :: IO ()
main = do
    chevalier_url <- getEnv "CHEVALIER_URL"
    chevalier_query_mvar <- newEmptyMVar

    asyncs <- replicateM 10 $
        async $ chevalier chevalier_url chevalier_query_mvar

    async $ watchThreads asyncs (chevalier chevalier_url chevalier_query_mvar)

    quickHttpServe $
        ifTop (writeBS docString) <|>
        route [ ("simple/search", simpleSearch chevalier_query_mvar) ]
  where
    -- Wait for any thread to explode, then restart it whilst logging the
    -- exception.
    watchThreads as restart_action = do
        (action, err) <- waitAnyCatch as

        case err of
            Right _ -> putStrLn "Thread exited normally (shouldn't happen)"
            Left e -> putStrLn $ "Thread exploded: " ++ show e

        let remaining_threads = filter (/= action) as
        a <- async restart_action

        threadDelay 1000000
        watchThreads (a:remaining_threads) restart_action

    docString = "This is the Vaultaire REST interface, you can find \
                \documentation in the wiki under TODO"
