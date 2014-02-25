{-# LANGUAGE OverloadedStrings #-}
module Chevalier where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.ByteString        (ByteString)
import           Data.Monoid            (mappend)
import           Data.ProtocolBuffers   hiding (field)
import           Data.Serialize
import           Data.Text.Internal     (Text)
import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as LazyBuilder
import           System.ZMQ4            hiding (source)
import           Types

-- | Chevalier communication thread, reads SourceQuery requsts from an mvar and
-- replies over the included mvar.
chevalier :: String -> MVar SourceQuery -> IO ()
chevalier chevalier_url query_mvar =
    withContext $ \c ->
        withSocket c Req $ \s -> do
            connect s chevalier_url
            forever $ do
                query <- takeMVar query_mvar
                result <- try $ do
                    send s [] $ encodeChevalierRequest $ buildChevalierRequest query
                    response <- decodeChevalierResponse <$> receive s
                    either error (return . buildSources) response
                putMVar (queryResponse query) result

encodeChevalierRequest :: ChevalierRequest -> ByteString
encodeChevalierRequest = runPut . encodeMessage

decodeChevalierResponse :: ByteString -> Either String ChevalierResponse
decodeChevalierResponse = runGet decodeMessage

buildSources :: ChevalierResponse -> [LazyText.Text]
buildSources r = map urlSafeSource (getField $ sources r)

urlSafeSource :: ChevalierSource -> LazyText.Text
urlSafeSource s =
  let tags = getField $ source s
      builder = foldl f "" tags
  in LazyBuilder.toLazyText builder
  where
    f acc (ChevalierTag k v) = acc
        <> (LazyBuilder.fromText $ getField v)
        <> "~"
        <> (LazyBuilder.fromText $ getField k)
        <> ","
    (<>) = mappend


buildChevalierRequest :: SourceQuery -> ChevalierRequest
buildChevalierRequest (SourceQuery q page _ ) = ChevalierRequest
    { requestTags = putField $ buildTags q
    , startPage = putField $ Just $ fromIntegral page
    , sourcesPerPage = putField $ Just 64
    }

buildTags  :: Text -> [ChevalierTag]
buildTags q = [ ChevalierTag { field = putField "*", value = putField q } ]
