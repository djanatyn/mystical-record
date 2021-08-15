{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Monad
import Data.Coerce
import Data.Maybe
import Database.Selda (Attr (..))
import qualified Database.Selda as DB
import qualified Database.Selda.SQLite as DBS
import Network.HTTP.Client
import Network.HTTP.Media hiding (Accept)
import Relude
import Servant.API
import Servant.API.Generic
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic
import Text.XML.HXT.CSS
import Text.XML.HXT.Core

data HTML

instance MimeUnrender HTML Text where
  mimeUnrender _ = Right . decodeUtf8

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

data ArchiveAPI r where
  ArchiveAPI ::
    { listDJ ::
        r :- "archives.php" :> QueryParam "dj" DJ
          :> Get '[HTML] Text
    } ->
    ArchiveAPI r
  deriving (Generic)

newtype DJ = DJ Text deriving (Show, ToHttpApiData, Generic)

data Broadcast where
  Broadcast ::
    { url :: Text,
      dj :: Text
    } ->
    Broadcast
  deriving (Show, Generic)

instance DB.SqlRow Broadcast

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
  return $ map (\url -> Broadcast {url = toText url, dj = coerce dj}) urls

fetchDJ :: DJ -> IO (Maybe [Broadcast])
fetchDJ dj = do
  archives <- runArchive $ listDJ archiveClient (Just dj)
  case archives of
    Left error -> return Nothing
    Right html -> Just <$> parseBroadcasts dj (toString html)

broadcasts :: DB.Table Broadcast
broadcasts = DB.table "broadcasts" [#url :- DB.primary]

runArchive :: ClientM a -> IO (Either ClientError a)
runArchive action = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager $ BaseUrl Http "radio.fobby.net" 80 "archives"
   in runClientM action env

archiveClient :: RunClient m => ArchiveAPI (AsClientT m)
archiveClient = genericClient @ArchiveAPI

initDatabase :: FilePath -> [Broadcast] -> IO ()
initDatabase path input = DBS.withSQLite path $ do
  DB.createTable broadcasts
  DB.insert_ broadcasts input

main :: IO ()
main = do
  archiveBroadcasts <- join . catMaybes <$> traverse fetchDJ knownDJs
  initDatabase "broadcasts.db" archiveBroadcasts
