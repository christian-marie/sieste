{-# LANGUAGE OverloadedStrings #-}

module Util where
import           Data.Aeson
import qualified Data.ByteString.Char8        as B
import           Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import           Data.Text.Lazy.Encoding      (decodeUtf8)
import           Snap.Core
import           Types.Util
import Data.ByteString(ByteString)
import Data.Text (Text)
import           Data.Text.Encoding           (decodeUtf8')
import           Data.ByteString.Lazy.Builder (string7)

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
