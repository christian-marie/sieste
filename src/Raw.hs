{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Raw where

import           Control.Applicative
import           Control.Concurrent           hiding (yield)
import           Control.Monad                (forever)
import           Control.Monad.IO.Class
import           Data.Aeson                   (encode, toJSON)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B
import qualified Data.ByteString.Lazy         as LB
import           Data.ByteString.Lazy.Builder (stringUtf8)
import           Data.ProtocolBuffers         (getField)
import           Pipes
import           Pipes.Concurrent
import           Snap.Core
import           Types.ReaderD                (DataBurst (..), Range (..),
                                               RangeQuery (..))
import           Util

raw :: MVar RangeQuery -> Snap ()
raw readerd_mvar = do
    tags <- getParam "source" >>= (\s -> case s of
        Just bs -> utf8Or400 bs >>= tagsOr400
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'source'")

    end <- getParam "end"
        >>= validateW64 (> 0) "end must be > 0" timeNow

    start <- getParam "start"
          >>= validateW64 (< end) "start must be < end" (return $ end - 86400)

    origin <- getParam "origin" >>= (\o -> case o of
        Just bs -> utf8Or400 bs
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    input <- liftIO $ do
        (output, input) <- spawn Single
        putMVar readerd_mvar $ RangeQuery tags start end origin output
        return input

    modifyResponse $ setContentType "application/json"
    writeBS "["
    runEffect $ for (fromInput input
                     >-> logExceptions
                     >-> unRange
                     >-> extractBursts
                     >-> jsonEncode
                     >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"
  where
    validateW64 check error_msg def user_input =
        case user_input of
            Just bs -> do
                let parsed = fromEpoch $ toInt bs
                check parsed `orFail` error_msg
                return parsed
            Nothing -> def

    -- Log exceptions, pass on Ranges
    logExceptions = forever $ await >>= either (lift . logException) yield

    -- Extract a DataBurst from a Range, stopping when Done
    unRange = do
        range <- await
        case range of Burst b -> yield b >> unRange
                      Done -> return ()

    -- Sort the DataBurst by time, passing on a list of DataFrames
    extractBursts = forever $ getField . frames <$> await >>= mapM_ yield

    jsonEncode = forever $ (encode . toJSON <$> await) >>= yield

    -- We want to prepend all but the first burst with a comma.
    addCommas is_first
        | is_first  = await >>= yield >> addCommas False
        | otherwise = do
            burst <- await
            yield $ LB.append "," burst
            addCommas False

toInt :: Integral a => ByteString -> a
toInt bs = maybe 0 (fromIntegral . fst) (B.readInteger bs)
