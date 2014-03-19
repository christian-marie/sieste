{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE RecordWildCards   #-}

module Util where
import           Control.Applicative
import           Data.Aeson
import           Data.Attoparsec.Text         (parseOnly, takeWhile1, (<*.))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B
import           Data.ByteString.Lazy.Builder (Builder, stringUtf8,
                                               toLazyByteString)
import           Data.Maybe
import           Data.Monoid                  ((<>))
import           Data.ProtocolBuffers         (getField, putField)
import           Data.Text                    (Text)
import           Data.Text.Encoding           (decodeUtf8')
import           Data.Text.Lazy.Encoding      (decodeUtf8)
import           Data.Word                    (Word64)
import           Pipes
import           Snap.Core
import           System.Clock                 (Clock (..), getTime, nsec, sec)
import           Types.ReaderD                (DataFrame (..), SourceTag (..),
                                               ValueType (..))
import           Types.Util

fromEpoch :: Int -> Word64
fromEpoch = fromIntegral . (* 1000000000)

toEpoch :: Word64 -> Int
toEpoch = fromIntegral . (`div` 1000000000)

writeJSON :: ToJSON a => a -> Snap ()
writeJSON a = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode $ toJSON a

writeError :: Int -> Builder -> Snap a
writeError code builder = do
    let pretty = PrettyError $ decodeUtf8 $ toLazyByteString builder
    modifyResponse $ setResponseCode code
    writeJSON pretty
    getResponse >>= finishWith

logException :: Show a => a -> Snap ()
logException = logError . B.pack . show

orFail :: Bool -> String -> Snap ()
orFail True _    = return ()
orFail False msg = writeError 400 $ stringUtf8 msg

utf8Or400 :: ByteString -> Snap Text
utf8Or400 = either conversionError return . decodeUtf8'
  where
    conversionError _ = writeError 400 $ stringUtf8 "Invalid UTF-8 in request"

tagsOr400 :: Text -> Snap [SourceTag]
tagsOr400 text =
    case parseOnly tagParser text of
        Left e ->
            writeError 400 $ stringUtf8 "failed to parse source: " <> stringUtf8 e
        Right s ->
            return s
  where
    tagParser = some $ SourceTag <$> k <*> v
      where
        k = putField <$> takeWhile1 (/= '~') <*. "~"
        v = putField <$> takeWhile1 (/= ',') <* optional ","

lerp :: Rational -> Rational -> Rational -> Rational
lerp a b alpha = ((1.0 - alpha) * a) + (alpha * b)

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

pointTime :: DataFrame -> Word64
pointTime = fromIntegral . getField . timestamp

getValue :: DataFrame -> Rational
getValue DataFrame{..}
    | getField payload == NUMBER = toRational $ fromJust $ getField valueNumeric
    | getField payload == REAL   = toRational $ fromJust $ getField valueMeasurement
    | otherwise                  = error "Unhandled data burst type"


timeNow :: MonadIO m => m Word64
timeNow = liftIO $ fmap fromIntegral $
    (+) <$> ((1000000000*) . sec) <*> nsec <$> getTime Realtime
