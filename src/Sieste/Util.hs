{-# LANGUAGE OverloadedStrings #-}

module Sieste.Util where
import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Builder (Builder, stringUtf8, toLazyByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Word (Word64)
import Pipes
import qualified Pipes.Prelude as Pipes
import Sieste.Types.Util
import Snap.Core
import System.Clock (Clock (..), getTime, nsec, sec)

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

timeNow :: MonadIO m => m Word64
timeNow = liftIO $ fmap fromIntegral $
    (+) <$> ((1000000000*) . sec) <*> nsec <$> getTime Realtime

toInt :: Integral a => ByteString -> a
toInt bs = maybe 0 (fromIntegral . fst) (B.readInteger bs)

validateW64 :: (Word64 -> Bool)  -- ^ function  to check if user's value is OK
            -> String            -- ^ error message if it is not okay
            -> Snap Word64       -- ^ action to retrieve default value
            -> Maybe ByteString  -- ^ possible user input
            -> Snap Word64
validateW64 check error_msg def user_input =
    case user_input of
        Just bs -> do
            let parsed = fromEpoch $ toInt bs
            check parsed `orFail` error_msg
            return parsed
        Nothing -> def

-- Useful pipes follow
jsonEncode :: (Monad m, ToJSON j) => Pipe j LB.ByteString m ()
jsonEncode = Pipes.map (encode . toJSON)

-- We want to prepend all but the first burst with a comma.
addCommas :: Monad m => Bool -> Pipe LB.ByteString LB.ByteString m ()
addCommas is_first
    | is_first  = await >>= yield >> addCommas False
    | otherwise = do
        burst <- await
        yield $ LB.append "," burst
        addCommas False

