{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Sieste.Chevalier (chevalier)
import Sieste.Interpolated (interpolated)
import Sieste.Raw (raw)
import Sieste.SimpleSearch (simpleSearch)
import Snap.Core
import Snap.Http.Server
import System.Environment (getEnv)

main :: IO ()
main = do
    chevalier_url <- getEnv "CHEVALIER_URL"
    chevalier_query_mvar <- newEmptyMVar
    let start_chevalier = chevalier chevalier_url chevalier_query_mvar

    chevalier_threads <- replicateM 16 $ async start_chevalier
    async $ watchThreads chevalier_threads start_chevalier

    quickHttpServe $
        ifTop (writeBS docString) <|>
        route [ ("simple/search", simpleSearch chevalier_query_mvar)
              , ("interpolated/:address", interpolated)
              , ("raw/:address", raw) ]

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

    docString = "<html>This is the Vaultaire REST interface, you can find \
                \documentation in the <a href=\"https://github.com/anchor/sieste\" \
                \target=\"_blank\">project readme</a>"
