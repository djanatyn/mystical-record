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

-- | Radio Starmen Archives are enumerated with a PHP script, "archives.php".
-- | The script takes a query parameter, `dj`, and returns HTML with a listing
-- | of broadcasts.
data ArchiveAPI r where
  ArchiveAPI ::
    { listDJ ::
        r :- "archives.php" :> QueryParam "dj" DJ
          :> Get '[HTML] Text
    } ->
    ArchiveAPI r
  deriving (Generic)

-- | There are several different DJs in the archive, all indexed by strings
newtype DJ = DJ Text deriving (Show, ToHttpApiData, Generic)

-- | The HTML returned by archives.php has a Content-Type which isn't
-- | known by servant: < Content-Type: text/html; charset=utf-8
data HTML

instance MimeUnrender HTML Text where
  mimeUnrender _ = Right . decodeUtf8

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

-- | Run HTTP requests against archiveAPI
runArchive :: ClientM a -> IO (Either ClientError a)
runArchive action = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager $ BaseUrl Http "radio.fobby.net" 80 "archives"
   in runClientM action env

-- | We know about several DJs that exist within the archive
knownDJs :: [DJ]
knownDJs =
  [ DJ "who",
    DJ "umbra",
    DJ "mon"
  ]

-- | The ArchiveAPI provides links to individual broadcasts
data BroadcastLink where
  BroadcastLink ::
    { url :: Text,
      dj :: Text
    } ->
    BroadcastLink
  deriving (Show, Generic)

instance DB.SqlRow BroadcastLink

type ApiHTML = String

-- | Download a BroadcastLink
downloadBroadcast :: BroadcastLink -> FilePath -> IO ()
downloadBroadcast (BroadcastLink {url, dj}) path = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest $ "http://radio.fobby.net/archives" ++ toString url
  response <-
    withResponse request manager $
      Network.HTTP.Client.responseBody
  writeFileBS path response

-- | Parse out [BroadcastLink] from ArchiveAPI response for a given DJ
parseBroadcasts :: DJ -> ApiHTML -> IO [BroadcastLink]
parseBroadcasts dj html = do
  urls <-
    runX $
      readString [withParseHTML yes] html
        >>> css ("a" :: String)
        >>> getAttrValue "href"
  return $ map (\url -> BroadcastLink {url = toText url, dj = coerce dj}) urls

-- | HTTP Client for ArchiveAPI
archiveClient :: RunClient m => ArchiveAPI (AsClientT m)
archiveClient = genericClient @ArchiveAPI

-- | Use archiveClient to fetch broadcast listings for a given DJ
fetchDJ :: DJ -> IO (Maybe [BroadcastLink])
fetchDJ dj = do
  archives <- runArchive $ listDJ archiveClient (Just dj)
  case archives of
    Left error -> return Nothing
    Right html -> Just <$> parseBroadcasts dj (toString html)

-- | SQLite table for BroadcastLink
broadcasts :: DB.Table BroadcastLink
broadcasts = DB.table "broadcasts" [#url :- DB.primary]

-- | Create database + tables
initDatabase :: FilePath -> [BroadcastLink] -> IO ()
initDatabase path input = DBS.withSQLite path $ do
  DB.createTable broadcasts
  DB.insert_ broadcasts input

-- | Fetch broadcasts listings for all known artists, add to database
main :: IO ()
main = do
  archiveBroadcasts <- join . catMaybes <$> traverse fetchDJ knownDJs
  initDatabase "broadcasts.db" archiveBroadcasts
