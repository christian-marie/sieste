{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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
import           Data.ByteString.Lazy.Builder (string7)
import           Data.List                    (sortBy)
import           Data.Maybe
import           Data.ProtocolBuffers         (getField)
import           Data.Word                    (Word64)
import           Pipes
import           Pipes.Concurrent
import           Snap.Core
import           System.Timeout               (timeout)
import           Types.Chevalier              (SourceQuery (..))
import           Types.ReaderD                (DataBurst (..), DataFrame (..),
                                               Range (..), RangeQuery (..),
                                               ValueType (..))
import           Util

simpleSearch :: MVar SourceQuery -> Snap ()
simpleSearch chevalier_mvar = do
    query <- utf8Or400 =<< fromMaybe "*" <$> getParam "q"
    page <- toInt <$> fromMaybe "0" <$> getParam "page"

    maybe_response <- liftIO $ do
        response_mvar <- newEmptyMVar
        putMVar chevalier_mvar $ SourceQuery query page response_mvar
        timeout chevalierTimeout $ takeMVar response_mvar

    either_response <- maybe timeoutError return maybe_response
    either chevalierError writeJSON either_response
  where
    chevalierTimeout = 10000000 -- 10 seconds

    chevalierError e = do
        logException e
        writeError 500 $ string7 "Exception talking to chevalier backend"

    timeoutError = do
        let msg = "Timed out talking to chevalier backend"
        logException msg
        writeError 500 $ string7 msg

interpolated :: MVar RangeQuery -> Snap ()
interpolated readerd_mvar = do
    -- The reader daemon provides no timestamp sorting within a chunk, but will
    -- provide sorting between chunks.
    --
    -- This means that the latest point (chronologically) in a burst will be no
    -- later than the first point in the next burst.
    --
    -- This allows us to stream the data the user chunk by chunk.

    tags <- tagsOr400 =<< utf8Or400 =<< fromJust <$> getParam "source"
    start <- fromEpoch . toInt <$> fromMaybe "0" <$> getParam "start"
    end <- fromEpoch . toInt <$> fromMaybe "0" <$> getParam "end"
    interval <- toInt <$> fromMaybe "0" <$> getParam "interval"

    input <- liftIO $ do
        (output, input) <- spawn Single
        putMVar readerd_mvar $ RangeQuery tags start end output
        return input

    modifyResponse $ setContentType "application/json"
    modifyResponse $ setBufferingMode False
    writeBS "["
    runEffect $ for (fromInput input
                     >-> logExceptions
                     >-> unRange
                     >-> sortBurst
                     >-> interpolate interval (fromIntegral start)
                     >-> jsonEncode
                     >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"
  where
    fromEpoch :: Int -> Word64
    fromEpoch = fromIntegral . (* 1000000000)

    toEpoch :: Word64 -> Int
    toEpoch = fromIntegral . (`div` 1000000000)
    -- Pipes are chained from top to bottom:

    -- Log exceptions, pass on Ranges
    logExceptions = forever $ await >>= either (lift . logException) yield

    -- Extract a DataBurst from a Range, stopping when Done
    unRange = do
        range <- await
        case range of Burst b -> yield b >> unRange
                      Done -> return ()

    -- Sort the DataBurst by time, passing on a list of DataFrames
    sortBurst = do
        unsorted <- getField . frames <$> await
        yield $ sortBy compareByTime unsorted
        sortBurst
      where
        compareByTime frame_a frame_b = compare (ts frame_a) (ts frame_b)
        ts = getField . timestamp

    interpolate interval now = do
        -- On our first run, we have to find an initial value to interpolate
        -- from.
        ps <- await
        case ps of
            (p:ps') -> emitAt now p ps'
            _      -> interpolate interval now
      where
        -- t    - the current interpolated target time we are trying to emit
        --         a value for
        -- p    - last known data point
        -- p:ps - next data points
        emitAt :: Monad m => Word64 -> DataFrame -> [DataFrame] -> Pipe [DataFrame] (Int, Double) m ()
        emitAt t p (p':ps) = do
            let p_time = getTime p
            if p_time < t
                then
                    -- If the next point is beyond the requested_time, we can
                    -- interpolate its value. If not, we need to look further
                    -- forward in the list
                    let p'_time = getTime p' in
                    if p'_time > t
                        then do
                            -- Obviously we have a match now and we can emit
                            -- this value. We go for Rational precision here as
                            -- we may be dealing with Word64s and I'm not sure
                            -- what kind of use cases we are dealing with.
                            --
                            -- If this turns out to be slow, we can use
                            -- Doubles.
                            let smalld = toRational $ p'_time - p_time
                            let bigd   = toRational $ p'_time - t
                            let lerped = lerp (getValue p) (getValue p') (smalld / bigd)
                            yield (toEpoch t, fromRational lerped)

                            -- Now look for the next interval
                            emitAt (t + interval) p (p':ps)
                        else
                            -- Seek forward
                            emitAt t p' ps
                else
                    -- This case should only be hit until our requested time
                    -- catches up to our first data point
                    emitAt (t + interval) p (p':ps)

        -- End of input, request another chunk
        emitAt t p [] = await >>= emitAt t p

        getTime :: DataFrame -> Word64
        getTime = fromIntegral . getField . timestamp


    jsonEncode = (encode . toJSON <$> await) >>= yield

    -- We want to prepend all but the first burst with a comma.
    addCommas is_first
        | is_first  = await >> addCommas False
        | otherwise = do
            burst <- await
            yield $ LB.append "," burst
            addCommas False

getValue :: DataFrame -> Rational
getValue DataFrame{..}
    | getField payload == NUMBER = toRational $ fromJust $ getField valueNumeric
    | getField payload == REAL   = toRational $ fromJust $ getField valueMeasurement

toInt :: Integral a => ByteString -> a
toInt bs = maybe 0 (fromIntegral . fst) (B.readInteger bs)
