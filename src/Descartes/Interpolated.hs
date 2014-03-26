{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Descartes.Interpolated where

import           Control.Concurrent           hiding (yield)
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Builder (stringUtf8)
import           Data.Word                    (Word64)
import           Descartes.Types.ReaderD      (DataFrame (..), RangeQuery (..))
import           Descartes.Util
import           Pipes
import           Pipes.Concurrent
import           Snap.Core

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

    origin' <- getParam "origin" >>= (\o -> case o of
        Just bs -> utf8Or400 bs
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    input <- liftIO $ do
        (output, input) <- spawn Single
        putMVar readerd_mvar $ RangeQuery tags start end origin' output
        return input

    modifyResponse $ setContentType "application/json"
    writeBS "["
    runEffect $ for (fromInput input
                     >-> logExceptions
                     >-> extractBursts
                     >-> interpolate interval (fromIntegral start) (fromIntegral end)
                     >-> jsonEncode
                     >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"


-- This pipe takes DataFrames as input, interpolating between the values to
-- output interpolated x,y tuples at given intervals, from now to end.
interpolate :: Word64 -> Word64 -> Word64
            -> Pipe DataFrame (Int, Double) Snap ()
interpolate interval now end
    | interval <= 0 = error "interval <= 0"
    | now > end = error "now > end"
    | otherwise = await >>= emitAt now Nothing
  where
    emitAt :: Word64    -- ^ The current requested time
           -> Maybe DataFrame -- ^ Maybe then next data point, to allow
                              --   multiple interpolated values between points
           -> DataFrame -- ^ The last known data point, initially the first.
           -> Pipe DataFrame (Int, Double) Snap ()
    emitAt t maybe_next p
        | t > end = return ()
        | p_time <- pointTime p
        , p_time <= t = do
            p' <- case maybe_next of
                Just n    -> return n
                Nothing   -> await

            let p'_time = pointTime p'
            -- Our first point is behind the requested time, which means that
            -- If the next point is beyond the requested_time, we can
            -- interpolate its value. If not, we need to look further
            -- forward in the list
            if p'_time >= t
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
                    let alpha | p'_time == t = 0
                              | p_time  == t = 1
                              | otherwise    = bigd / smalld
                    let lerped = lerp (getValue p') (getValue p) alpha
                    yield (toEpoch t, fromRational lerped)

                    -- Now look for the next interval, we must keep the current
                    -- point in case we have to 'invent' several interpolated
                    -- points between this one and the next.
                    emitAt (t + interval) (Just p') p
                else
                    -- Seek forward
                    emitAt t Nothing p'
        | p_time <- pointTime p, p_time > t =
            -- Our point is ahead of the requested time, this should only
            -- happen once: initially. We catch up in one iteration by
            -- calculating the next valid interval given this first point.
            let first = ((p_time `div` interval) + 1) * interval in
                emitAt first Nothing p
        | otherwise = error "emitAt: impossible"


lerp :: Rational -> Rational -> Rational -> Rational
lerp a b alpha = ((1.0 - alpha) * a) + (alpha * b)
