{-# LANGUAGE OverloadedStrings #-}
module Simple where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as B
import           Data.Maybe
import           Snap.Core
import           Types

simpleSearch :: MVar SourceQuery -> Snap ()
simpleSearch query_mvar = do
    query <- fromMaybe "*" <$> getParam "q"
    page <- toInt<$> fromMaybe "0" <$> getParam "page"
    response_mvar <- liftIO newEmptyMVar
    liftIO $ putMVar query_mvar $ SourceQuery query page response_mvar
    response <- liftIO $ takeMVar response_mvar
    writeBS response
  where
    toInt bs = maybe 0 fst (B.readInteger bs)
