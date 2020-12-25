{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Coerce
import Data.Text
import Network.HTTP.Client
import Network.HTTP.Media hiding (Accept)
import Servant.API
import Servant.API.Generic
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic
import Text.XML.HXT.CSS
import Text.XML.HXT.Core

newtype DJ = DJ Text deriving (Show, ToHttpApiData)

data HTML

instance MimeUnrender HTML Text where
  mimeUnrender _ = Right . pack . BSC.unpack

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

data ArchiveAPI r = ArchiveAPI
  { listDJ ::
      r :- "archives.php" :> QueryParam "dj" DJ
        :> Get '[HTML] Text
  }
  deriving (Generic)

newtype Broadcast = Broadcast [String] deriving (Show)

runArchive :: ClientM a -> IO (Either ClientError a)
runArchive action = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager $ BaseUrl Http "radio.fobby.net" 80 "archives"
   in runClientM action env

archiveClient :: RunClient m => ArchiveAPI (AsClientT m)
archiveClient = genericClient @ArchiveAPI

main :: IO ()
main = do
  archives <- runArchive $ listDJ archiveClient (Just $ DJ "umbra")
  case archives of
    Left error -> print error
    Right archives -> do
      broadcast <- parseBroadcast $ unpack archives
      print broadcast

parseBroadcast :: String -> IO Broadcast
parseBroadcast html =
  coerce . runX $
    readString [withParseHTML yes] html
      >>> css ("a" :: String)
      >>> getAttrValue "href"
