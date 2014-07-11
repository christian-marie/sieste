--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Sieste.Interpolated where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy.Builder (stringUtf8)
import Data.Maybe
import Data.ReinterpretCast
import Data.String
import Data.Word (Word64)
import Marquise.Client
import Numeric(fromRat)
import Pipes
import qualified Pipes.Prelude as Pipes
import Sieste.Classes
import Sieste.Types.SimplePoint
import Sieste.Util
import Snap.Core

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
    test_mode <- isJust <$> getParam "test"
    let stream = if test_mode
                    then hoist (return . runIdentity) (readPoints address start end origin)
                    else hoist liftIO (readPoints address start end origin)

    as_double  <- isJust <$> getParam "as_double"
    let json_processor = if as_double
                            then Pipes.map AsDouble >-> jsonEncode
                            else jsonEncode

    modifyResponse $ setContentType "application/json"
    writeBS "["
    runEffect $ for (stream
                    >-> interpolate as_double interval (fromIntegral start) (fromIntegral end)
                    >-> json_processor
                    >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"

interpolate :: Bool -> Word64 -> Word64 -> Word64 -> Pipe SimplePoint SimplePoint Snap ()
interpolate as_double interval now end
    | interval <= 0 = error "interval <= 0"
    | now > end = error "now > end"
    | otherwise = do
        left_p <- await
        right_p <- await
        emitAt now left_p right_p
  where
    emitAt :: Word64 -- ^ The current requested time
           -> SimplePoint -- ^ The 'left' data point
           -> SimplePoint -- ^ The 'right' (next) data point.
           -> Pipe SimplePoint SimplePoint Snap ()
    emitAt t left_p right_p
        | t > end = return () -- could yield lerped value at end delta
        | simpleTime left_p <= t =
            -- Our first point is behind the requested time, which
            -- means that If the next point is beyond the
            -- requested_time, we can interpolate its value. If not, we
            -- need to look further forward in the list
            let left_time = simpleTime left_p in
            let right_time = simpleTime right_p in
            if right_time >= t
                then do
                    -- Obviously we have a match now and we can emit
                    -- this value. We go for Rational precision here as
                    -- we may be dealing with Word64s and I'm not sure
                    -- what kind of use cases we are dealing with.
                    --
                    -- If this turns out to be slow, we can use
                    -- Doubles.
                    let smalld = toRational $ right_time - left_time
                    let bigd = toRational $ right_time - t
                    let alpha | right_time == t = 0
                              | left_time == t = 1
                              | otherwise = bigd / smalld
                    let insert = if as_double then toRational . wordToDouble else fromIntegral
                    let extract = if as_double then doubleToWord . fromRat else round
                    let lerped = lerp (insert $ simplePayload right_p)
                                      (insert $ simplePayload left_p)
                                      alpha
                    -- Reuse either point's address
                    yield left_p{simpleTime = t, simplePayload = extract lerped}

                    -- Now look for the next interval, we must keep the
                    -- current points in case we have to allow
                    -- multiple interpolated points between this one and
                    -- the next.
                    emitAt (t + interval) left_p right_p
                else
                    -- Seek forward:
                    --   * a new right_p is awaited
                    --   * the current left_p becomes the new right_p
                    await >>= emitAt t right_p
        | simpleTime left_p > t =
            -- Our leftmost point is ahead of the requested time, this should
            -- only happen once: initially. We catch up in one iteration by
            -- calculating the next valid interval given this first point.
            let first = ((simpleTime left_p `div` interval) + 1) * interval in
                emitAt first left_p right_p
        | otherwise = error "emitAt: impossible"


-- | Linear interpolation between a and b at ratio alpha (between 0 and 1)
lerp :: Rational -> Rational -> Rational -> Rational
lerp a b alpha = ((1.0 - alpha) * a) + (alpha * b)
