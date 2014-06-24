{-# LANGUAGE OverloadedStrings   #-}

module Sieste.Interpolated where

import           Control.Applicative
import           Control.Concurrent           hiding (yield)
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Builder (stringUtf8)
import           Data.Word                    (Word64)
import           Sieste.Util
import           Sieste.Types.SimplePoint
import           Pipes
import           Snap.Core
import           Sieste.Classes
import           Sieste.IOPointReader
import           Marquise.Classes
import           Marquise.Types
import           Vaultaire.Types
import           Control.Monad.Identity
import           Data.String
import qualified Data.ByteString.Char8        as S
import qualified Pipes.Prelude                as Pipes

interpolated :: Snap ()

interpolated = do
    -- The reader daemon provides no timestamp sorting within a chunk, but will
    -- provide sorting between chunks.
    --
    -- This means that the latest point (chronologically) in a burst will be no
    -- later than the first point in the next burst.
    --
    -- This allows us to stream the data the user chunk by chunk.
    address <- getParam "address" >>= (\s -> case s of
        Just bs -> return . fromString . S.unpack $ bs
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'address'")

    end <- getParam "end"
        >>= validateW64 (> 0) "end must be > 0" timeNow

    start <- getParam "start"
          >>= validateW64 (< end) "start must be < end" (return $ end - 86400)

    interval <- getParam "interval"
             >>= validateW64 (> 0)  "interval must be > 0" (return $ fromEpoch 60)

    origin <- getParam "origin" >>= (\o -> case o of
        Just bs -> either (const $ writeError 400 $ stringUtf8 "Invalid origin")
                          return
                          (makeOrigin bs)
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    -- If the user would like to use the test producer, we can 'hoist' that
    -- from identity to IO through the Pipe monad stack.
    --
    -- Otherwise, we simply lift to IO and ask the Reader daemon for actual
    -- points.
    input <- getParam "test" >>= (\o -> return $ case o of
        Just _  -> hoist (return . runIdentity) (readPoints address start end origin)
        Nothing -> hoist liftIO (readPoints address start end origin))
    
    makeJSON <- getParam "as_double" >>= (\o -> return $ case o of
        Just _  -> Pipes.map AsDouble >-> jsonEncode
        Nothing -> jsonEncode)
    
    modifyResponse $ setContentType "application/json"
    writeBS "["
    runEffect $ for (input
                    >-> interpolate interval (fromIntegral start) (fromIntegral end)
                    >-> makeJSON
                    >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"

interpolate :: Word64 -> Word64 -> Word64 -> Pipe SimplePoint SimplePoint Snap ()
interpolate interval now end 
    | interval <= 0 = error "interval <= 0"
    | now > end = error "now > end"
    | otherwise = undefined
{-

    producer <- getParam "test" >>= (\o -> case o of
        Just _ ->
            hoist (return . runIdentity) readPoints -- KM (or 'undefined' if types don't match)
        Nothing ->
            liftIO readPoints)

    input <- liftIO $ do
        (output, input) <- spawn Single
        --putMVar readerd_mvar $ RangeQuery address start end origin output
        -- return input
        readPoints address start end origin

    modifyResponse $ setContentType "application/json"
    writeBS "["
    runEffect $ for (producer
                     >-> interpolate interval (fromIntegral start) (fromIntegral end)
                     >-> jsonEncode
                     >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"
-- KM - this method requires removal of kickback to RationalFrame, etc
--
-- This pipe takes SimplePoints as input, interpolating between the values to
-- output interpolated x,y tuples at given intervals, from now to end.
--
-- Control can transfer to count in the case of tryAwaitRationalBurst not
-- getting a rational.
interpolate :: Word64 -> Word64 -> Word64
            -> Pipe SimplePoint (Int, Double) Snap ()
interpolate interval now end
    | interval <= 0 = error "interval <= 0"
    | now > end = error "now > end"
    | otherwise = tryAwaitRationalBurst interval now end (emitAt now Nothing)
  where
    emitAt :: Word64              -- ^ The current requested time
           -> Maybe SimplePoint -- ^ Maybe the next data point, to allow
                                  --   multiple interpolated values between
                                  --   points
           -> SimplePoint       -- ^ The last known data point
           -> Pipe SimplePoint (Int, Double) Snap ()
    emitAt t maybe_next p
        | t > end = return () -- could yield lerped value at end delta
        | p_time <- pointTime p
        , p_time <= t =
            case maybe_next of
                Just p' -> do
                    let p'_time = pointTime p'
                    -- Our first point is behind the requested time, which
                    -- means that If the next point is beyond the
                    -- requested_time, we can interpolate its value. If not, we
                    -- need to look further forward in the list
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
                            let lerped = lerp (toRational p')
                                              (toRational p)
                                              alpha
                            yield (SimplePoint (toEpoch t) (fromRational lerped))

                            -- Now look for the next interval, we must keep the
                            -- current point in case we have to 'invent'
                            -- several interpolated points between this one and
                            -- the next.
                            emitAt (t + interval) (Just p') p
                        else
                            -- Seek forward
                            emitAt t Nothing p'
                Nothing -> return () -- KM ??
--                    tryAwaitRationalBurst interval now end
  --                                        (\new -> emitAt t (Just new) p)
        | p_time <- pointTime p, p_time > t =
            -- Our point is ahead of the requested time, this should only
            -- happen once: initially. We catch up in one iteration by
            -- calculating the next valid interval given this first point.
            let first = ((p_time `div` interval) + 1) * interval in
                emitAt first Nothing p
        | otherwise = error "emitAt: impossible"


lerp :: Rational -> Rational -> Rational -> Rational
lerp a b alpha = ((1.0 - alpha) * a) + (alpha * b)
-}
