{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simple where

import           Control.Applicative
import           Control.Concurrent           hiding (yield)
import           Control.Monad                (forever)
import           Control.Monad.IO.Class
import           Data.Aeson                   (encode, toJSON)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as LB
import           Data.ByteString.Lazy.Builder (stringUtf8)
import           Data.Maybe
import           Data.ProtocolBuffers         (getField)
import           Pipes
import           Pipes.Concurrent
import           Snap.Core
import           System.Timeout               (timeout)
import           Types.Chevalier              (SourceQuery (..))
import           Types.ReaderD                (DataBurst (..), Range (..),
                                               RangeQuery (..))
import           Util

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
        "BENHUR" -> return $ Just $ Right $ ["wave~sine"]
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

interpolated :: MVar RangeQuery -> Snap ()
interpolated readerd_mvar = do
    -- The reader daemon provides no timestamp sorting within a chunk, but will
    -- provide sorting between chunks.
    --
    -- This means that the latest point (chronologically) in a burst will be no
    -- later than the first point in the next burst.
    --
    -- This allows us to stream the data the user chunk by chunk.


    tags <- getParam "source" >>= (\s -> case s of
        Just bs -> utf8Or400 bs >>= tagsOr400
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'source'")

    end <- getParam "end"
        >>= validateW64 (> 0) "end must be > 0" timeNow

    start <- getParam "start"
          >>= validateW64 (< end) "start must be < end" (return $ end - 86400)

    interval <- getParam "interval"
             >>= validateW64 (> 0)  "interval must be > 0" (return $ fromEpoch 60)

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
                     >-> unRange
                     >-> extractBursts
                     >-> interpolate interval (fromIntegral start) (fromIntegral end)
                     >-> jsonEncode
                     >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"
  where
    validateW64 check error_msg def user_input =
        case user_input of
            Just bs -> do
                let parsed = fromEpoch $ toInt bs
                check parsed `orFail` error_msg
                return parsed
            Nothing -> def

    -- Log exceptions, pass on Ranges
    logExceptions = forever $ await >>= either (lift . logException) yield

    -- Extract a DataBurst from a Range, stopping when Done
    unRange = do
        range <- await
        case range of Burst b -> yield b >> unRange
                      Done -> return ()

    -- Sort the DataBurst by time, passing on a list of DataFrames
    extractBursts = getField . frames <$> await >>= mapM_ yield

    jsonEncode = (encode . toJSON <$> await) >>= yield >> jsonEncode

    -- We want to prepend all but the first burst with a comma.
    addCommas is_first
        | is_first  = await >>= yield >> addCommas False
        | otherwise = do
            burst <- await
            yield $ LB.append "," burst
            addCommas False

toInt :: Integral a => ByteString -> a
toInt bs = maybe 0 (fromIntegral . fst) (B.readInteger bs)
