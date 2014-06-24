{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sieste.Raw where

import           Control.Concurrent           hiding (yield)
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Builder (stringUtf8)
import           Sieste.Types.ReaderD      (RangeQuery (..))
import           Sieste.Util
import           Pipes
import           Pipes.Concurrent
import           Snap.Core
import           Marquise.Classes
import           Marquise.Types
import           Marquise.Client
import           Vaultaire.Types
import           Sieste.Classes
import           Control.Monad.Identity
import Data.String
import qualified Data.ByteString.Char8 as S
import Sieste.IdentityPointReader ()
import Sieste.IOPointReader ()
import qualified Pipes.Prelude as Pipes
import qualified Data.Aeson as A
import qualified Data.Vector as V
import Data.ReinterpretCast

newtype AsDouble = AsDouble SimplePoint

instance A.ToJSON AsDouble where
    toJSON (AsDouble (SimplePoint _ time payload)) =
        A.Array $ V.fromList [ A.toJSON time, A.toJSON (wordToDouble payload) ]

instance A.ToJSON SimplePoint where
    toJSON (SimplePoint a b c) = undefined

raw :: MVar RangeQuery -> Snap ()
raw readerd_mvar = do
    address <- getParam "address"  >>= (\o -> case o of 
        Just bs -> return . fromString . S.unpack $ bs
        Nothing -> writeError 400 $ stringUtf8 "Must specifiy 'source'")

    end <- getParam "end"
        >>= validateW64 (> 0) "end must be > 0" timeNow

    start <- getParam "start"
          >>= validateW64 (< end) "start must be < end" (return $ end - 86400)

    origin <- getParam "origin" >>= (\o -> case o of
        Just bs -> either (const $ writeError 400 $ stringUtf8 "Invalid origin")
                          return
                          (makeOrigin bs)
        Nothing -> writeError 400 $ stringUtf8 "Must specify 'origin'")

    input <- getParam "test" >>= (\o -> case o of
        Just _ -> return $ hoist (return . runIdentity) (readPoints address start end origin)
        Nothing -> return $ hoist liftIO (readPoints address start end origin))

    makeJSON <- getParam "as_double" >>= (\o -> return $ case o of
        Just _ -> Pipes.map AsDouble >-> jsonEncode
        Nothing -> jsonEncode)

    modifyResponse $ setContentType "application/json"
    writeBS "["
    runEffect $ for (input
                     >-> makeJSON
                     >-> addCommas True)
                    (lift . writeLBS)
    writeBS "]"
