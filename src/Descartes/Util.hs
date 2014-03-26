{-# LANGUAGE OverloadedStrings #-}

module Descartes.Util where
import           Control.Applicative
import           Control.Exception            (SomeException)
import           Control.Monad
import           Data.Aeson
import           Data.Attoparsec.Text         (parseOnly, takeWhile1, (<*.))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as LB
import           Data.ByteString.Lazy.Builder (Builder, stringUtf8,
                                               toLazyByteString)
import           Data.Monoid                  ((<>))
import           Data.ProtocolBuffers         (getField, putField)
import           Data.Text                    (Text)
import           Data.Text.Encoding           (decodeUtf8')
import           Data.Text.Lazy.Encoding      (decodeUtf8)
import           Data.Word                    (Word64)
import           Descartes.Types.ReaderD      (DataBurst (..), DataFrame (..),
                                               SourceTag (..))
import           Descartes.Types.Util
import           Pipes
import           Snap.Core
import           System.Clock                 (Clock (..), getTime, nsec, sec)

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

pointTime :: DataFrame -> Word64
pointTime = fromIntegral . getField . timestamp

timeNow :: MonadIO m => m Word64
timeNow = liftIO $ fmap fromIntegral $
    (+) <$> ((1000000000*) . sec) <*> nsec <$> getTime Realtime

toInt :: Integral a => ByteString -> a
toInt bs = maybe 0 (fromIntegral . fst) (B.readInteger bs)

validateW64 :: (Word64 -> Bool)  -- ^ functon  to check if user's value is OK
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

-- Log exceptions, pass on Ranges
logExceptions ::  Pipe (Either SomeException DataBurst) DataBurst Snap ()
logExceptions = forever $ await >>= either (lift . logException) yield

-- Yield the frames from a burst one by one, no need to sort as readerd does
-- that for us.
extractBursts :: Monad m => Pipe DataBurst DataFrame m ()
extractBursts = forever $ getField . frames <$> await >>= mapM_ yield

jsonEncode :: (Monad m, ToJSON j) => Pipe j LB.ByteString m ()
jsonEncode = forever $ (encode . toJSON <$> await) >>= yield

-- We want to prepend all but the first burst with a comma.
addCommas :: Monad m => Bool -> Pipe LB.ByteString LB.ByteString m ()
addCommas is_first
    | is_first  = await >>= yield >> addCommas False
    | otherwise = do
        burst <- await
        yield $ LB.append "," burst
        addCommas False

