{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Descartes.Raw where

import           Control.Concurrent           hiding (yield)
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Builder (stringUtf8)
import           Descartes.Types.ReaderD      (RangeQuery (..))
import           Descartes.Util
import           Pipes
import           Pipes.Concurrent
import           Snap.Core

raw :: MVar RangeQuery -> Snap ()
raw readerd_mvar = do
    tags <- getParam "source" >>= (\s -> case s of
        Just bs -> utf8Or400 bs >>= tagsOr400
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'source'")

    end <- getParam "end"
        >>= validateW64 (> 0) "end must be > 0" timeNow

    start <- getParam "start"
          >>= validateW64 (< end) "start must be < end" (return $ end - 86400)

    origin <- getParam "origin" >>= (\o -> case o of
        Just bs -> utf8Or400 bs
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    input <- liftIO $ do
        (output, input) <- spawn Single
        putMVar readerd_mvar $ RangeQuery tags start end origin output
        return input

    modifyResponse $ setContentType "application/json"
    writeBS "["
    runEffect $ for (fromInput input
                     >-> logExceptions
                     >-> extractBursts
                     >-> jsonEncode
                     >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"
