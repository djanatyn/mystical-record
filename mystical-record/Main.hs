{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Coerce
import Data.Maybe
import Data.Text
import Database.Selda
import Database.Selda.SQLite
import Network.HTTP.Client
import Network.HTTP.Media hiding (Accept)
import Servant.API
import Servant.API.Generic
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic
import Text.XML.HXT.CSS
import Text.XML.HXT.Core

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

newtype DJ = DJ Text deriving (Show, ToHttpApiData, Generic)

data Broadcast = Broadcast {url :: Text, dj :: Text} deriving (Show, Generic)

instance SqlRow Broadcast

knownDJs :: [DJ]
knownDJs =
  [ DJ "who",
    DJ "umbra",
    DJ "mon"
  ]

parseBroadcasts :: DJ -> String -> IO [Broadcast]
parseBroadcasts dj html = do
  urls <-
    runX $
      readString [withParseHTML yes] html
        >>> css ("a" :: String)
        >>> getAttrValue "href"
  return [Broadcast {url = pack url, dj = coerce dj} | url <- urls]

fetchDJ :: DJ -> IO (Maybe [Broadcast])
fetchDJ dj = do
  archives <- runArchive $ listDJ archiveClient (Just dj)
  case archives of
    Left error -> return Nothing
    Right html -> Just <$> parseBroadcasts dj (unpack html)

broadcasts :: Table Broadcast
broadcasts = table "broadcasts" [#url :- primary]

runArchive :: ClientM a -> IO (Either ClientError a)
runArchive action = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager $ BaseUrl Http "radio.fobby.net" 80 "archives"
   in runClientM action env

archiveClient :: RunClient m => ArchiveAPI (AsClientT m)
archiveClient = genericClient @ArchiveAPI

main :: IO ()
main = do
  archiveBroadcasts <- join . catMaybes <$> traverse fetchDJ knownDJs
  withSQLite "broadcasts.db" $ do
    createTable broadcasts
    insert_ broadcasts archiveBroadcasts
