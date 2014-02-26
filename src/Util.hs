{-# LANGUAGE OverloadedStrings #-}

module Util where
import           Control.Applicative
import           Data.Aeson
import           Data.Attoparsec.Text         (parseOnly, takeWhile1, (<*.))
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B
import           Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import           Data.ByteString.Lazy.Builder (string7)
import           Data.Monoid                  ((<>))
import           Data.ProtocolBuffers         (putField)
import           Data.Text                    (Text)
import           Data.Text.Encoding           (decodeUtf8')
import           Data.Text.Lazy.Encoding      (decodeUtf8)
import           Snap.Core
import           Types.ReaderD                (SourceTag (..))
import           Types.Util

writeJSON :: ToJSON a => a -> Snap ()
writeJSON lbs = do
    modifyResponse $ setContentType "application/json"
    writeLBS $ encode $ toJSON lbs

writeError :: Int -> Builder -> Snap a
writeError code builder = do
    let pretty = PrettyError $ decodeUtf8 $ toLazyByteString builder
    modifyResponse $ setResponseCode code
    writeJSON pretty
    getResponse >>= finishWith

logException :: Show a => a -> Snap ()
logException = logError . B.pack . show

utf8Or400 :: ByteString -> Snap Text
utf8Or400 = either conversionError return . decodeUtf8'
  where
    conversionError _ = writeError 400 $ string7 "Invalid UTF-8 in q param"

tagsOr400 :: Text -> Snap [SourceTag]
tagsOr400 text =
    case parseOnly tagParser text of
        Left e ->
            writeError 400 $ string7 "failed to parse source: " <> string7 e
        Right s ->
            return s
  where
    tagParser = some $ SourceTag <$> k <*> v
      where
        k = putField <$> takeWhile1 (/= '~') <*. "~"
        v = putField <$> takeWhile1 (/= ',') <* (optional ",")



