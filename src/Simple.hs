{-# LANGUAGE OverloadedStrings #-}
module Simple where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8        as B
import           Data.ByteString.Lazy.Builder (string7)
import           Data.Maybe
import           Data.Text.Encoding           (decodeUtf8')
import           Snap.Core
import           Types
import           Util                         (logException, writeError,
                                               writeJSON)

simpleSearch :: MVar SourceQuery -> Snap ()
simpleSearch query_mvar = do
    query <- fromMaybe "*" <$> getParam "q"
    page <- toInt <$> fromMaybe "0" <$> getParam "page"
    utf_query <- either conversionError return $ decodeUtf8' query

    response <- liftIO $ do
        response_mvar <- newEmptyMVar
        putMVar query_mvar $ SourceQuery utf_query page response_mvar
        takeMVar response_mvar

    either chevalierError writeJSON response
  where
    toInt bs = maybe 0 fst (B.readInteger bs)

    conversionError e = do
        logException e
        writeError 400 $ string7 "Invalid UTF-8 in q param"

    chevalierError e = do
        logException e
        writeError 500 $ string7 "Exception talking to chevalier backend"
