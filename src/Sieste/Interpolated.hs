{-# LANGUAGE OverloadedStrings #-}

module Sieste.Interpolated where

import Control.Applicative
import Control.Concurrent hiding (yield)
import Control.Monad.Identity
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy.Builder (stringUtf8)
import Data.String
import Data.Word (Word64)
import Marquise.Classes
import Marquise.Types
import Pipes
import qualified Pipes.Prelude as Pipes
import Sieste.Classes
import Sieste.IOPointReader
import Sieste.Types.SimplePoint
import Sieste.Util
import Snap.Core
import Vaultaire.Types

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
    | otherwise = undefined -- this needs to do the interpolation magic.

{-
-- Original lerp function
lerp :: Rational -> Rational -> Rational -> Rational
lerp a b alpha = ((1.0 - alpha) * a) + (alpha * b)
-}
