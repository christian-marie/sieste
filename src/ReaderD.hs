{-# LANGUAGE RecordWildCards #-}
module ReaderD where

import           Codec.Compression.LZ4 (decompress)
import           Control.Applicative   ((<$>))
import           Control.Concurrent    hiding (yield)
import           Control.Exception
import           Control.Monad         (forever)
import qualified Data.ByteString       as B
import           Data.ProtocolBuffers  hiding (field)
import           Data.Serialize        (runGet, runPut)
import           Data.Text.Encoding    (encodeUtf8)
import           Pipes
import           Pipes.Concurrent      (toOutput)
import           System.ZMQ4           hiding (source)
import           Types.ReaderD

readerd :: String -> MVar RangeQuery -> IO ()
readerd readerd_url query_mvar =
    withContext $ \c ->
        withSocket c Dealer $ \s ->
            forever $ processRequest s
  where
    processRequest s = do
        connect s readerd_url
        q@(RangeQuery _ _ _ origin output) <- takeMVar query_mvar
        let request = rangeQueryToRequestMulti q
        send s [SendMore] $ encodeUtf8 origin
        send s [] request

        runEffect $ yieldRanges s >-> toOutput output
        disconnect s readerd_url

    rangeQueryToRequestMulti RangeQuery{..} =
      let requests = [RequestSource tags start end]
          tags     = putField rangeSource
          start    = putField $ fromIntegral rangeStart
          end      = putField $ Just $ fromIntegral rangeEnd
      in encodeRequestMulti $ RequestMulti $ putField requests

    encodeRequestMulti = runPut . encodeMessage

    yieldRanges s = do
        either_msg <- lift . try $ do
            result <- poll readerdTimeout [Sock s [In] Nothing]
            if (null . head) result
                then throwIO ZMQTimeout -- timeout, bail
                else decompress <$> receive s
                     >>= maybe (throwIO DecompressionFailure) return

        case either_msg of
            -- On failure, pass the exception on and give up, more robust
            -- handling can be evaluated when we start to uncover handleable
            -- exceptions.
            Left e -> do
                yield $ Left e
            Right msg -> if B.null msg
                then yield $ Right Done
                -- If the burst cannot be decoded, we may as well give up,
                -- chances are someone is talking the wrong protocol
                else case decodeBurst msg of
                    Left e  -> yield $ Left $ toException $ BurstDecodeFailure e
                    Right b ->  do
                        yield $ Right $ Burst b
                        yieldRanges s

    decodeBurst = runGet decodeMessage

    readerdTimeout = 30000 -- 30 seconds
