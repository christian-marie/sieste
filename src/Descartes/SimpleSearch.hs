{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Descartes.SimpleSearch where

import           Control.Applicative
import           Control.Concurrent           hiding (yield)
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Builder (stringUtf8)
import           Data.Maybe
import           Data.Text                    (breakOnAll)
import           Descartes.Types.Chevalier    (SourceQuery (..))
import           Descartes.Util
import           Snap.Core
import           System.Timeout               (timeout)

simpleSearch :: MVar SourceQuery -> Snap ()
simpleSearch chevalier_mvar = do
    query <- utf8Or400 =<< fromMaybe "*" <$> getParam "q"
    page <- toInt <$> fromMaybe "0" <$> getParam "page"
    page_size <- toInt <$> fromMaybe "64" <$> getParam "page_size"

    origin <- getParam "origin" >>= (\o -> case o of
        Just bs -> utf8Or400 bs
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    maybe_response <- liftIO $ case origin of
        -- Demo data on "BENHUR"
        "BENHUR" ->
            -- The sine waves *really* bothered someone when she didn't ask for
            -- them. This appeases her. Note that *wave still does not work. I
            -- am hugely sorry for this Katie.
            return $ Just $ Right $
                case query of
                    "*"    -> ["wave~sine"]
                    search -> case breakOnAll search "wave~sine" of
                            [] -> []
                            _  -> ["wave~sine"]
        -- Otherwise, actually make the request to chevalier
        _        -> do
            response_mvar <- newEmptyMVar
            putMVar chevalier_mvar $ SourceQuery query page page_size origin response_mvar
            timeout chevalierTimeout $ takeMVar response_mvar

    either_response <- maybe timeoutError return maybe_response
    either chevalierError writeJSON either_response
  where
    chevalierTimeout = 10000000 -- 10 seconds

    chevalierError e = do
        logException e
        writeError 500 $ stringUtf8 "Exception talking to chevalier backend"

    timeoutError = do
        let msg = "Timed out talking to chevalier backend"
        logException msg
        writeError 500 $ stringUtf8 msg

