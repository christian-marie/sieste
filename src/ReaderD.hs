{-# LANGUAGE RecordWildCards #-}
module ReaderD where

import           Control.Concurrent   hiding (yield)
import           Control.Exception
import           Control.Monad        (forever)
import qualified Data.ByteString      as B
import           Data.ProtocolBuffers hiding (field)
import           Data.Serialize       (runGet, runPut)
import           Pipes
import           Pipes.Concurrent     (performGC, toOutput)
import           System.ZMQ4          hiding (source)
import           Types.ReaderD

readerd :: String -> MVar RangeQuery -> IO ()
readerd readerd_url query_mvar =
    withContext $ \c -> withSocket c Dealer $ \s -> do
        setReceiveTimeout readerdTimeout s
        connect s readerd_url
        forever $ do
            q@(RangeQuery _ _ _ output) <- takeMVar query_mvar
            let request = rangeQueryToRequestMulti q
            send s [] request

            runEffect $ yieldRanges s >-> toOutput output
            performGC
  where
    rangeQueryToRequestMulti RangeQuery{..} =
      let requests = [RequestSource tags start end]
          tags     = putField rangeSource
          start    = putField $ fromIntegral $ rangeStart
          end      = putField $ Just $ fromIntegral rangeEnd
      in encodeRequestMulti $ RequestMulti $ putField requests

    encodeRequestMulti = runPut . encodeMessage
    yieldRanges s = do
        either_msg <- lift . try $ receive s
        case either_msg of
            -- On failure, pass the exception on and give up, more robust
            -- handling can be evaluated when we start to uncover handleable
            -- exceptions.
            Left e -> do
                yield $ Left e
            Right msg ->
                if B.null msg
                then yield $ Right Done
                -- If the burst cannot be decoded, we may as well give up,
                -- chances are someone is talking the wrong protocol
                else yield $ either (Left . toException . BurstDecodeFailure)
                                    (Right . Burst)
                                    (decodeBurst msg)

    decodeBurst = runGet decodeMessage

    readerdTimeout = restrict (30000 :: Int) -- 30 seconds
