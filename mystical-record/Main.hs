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

import Colog.Core (LogAction (..), withLogStringFile, (&>))
import Colog.Core.IO (logStringStdout)
import Control.Exception
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Database.Selda (Attr (..), SeldaError)
import qualified Database.Selda as DB
import qualified Database.Selda.SQLite as DBS
import Network.Download (openURI)
import Network.HTTP.Client
import Network.HTTP.Media hiding (Accept)
import Relude
import Servant.API
import Servant.API.Generic
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)
import Text.XML.HXT.CSS
import Text.XML.HXT.Core

-- | Timestamp each log line
timestamp :: String -> IO String
timestamp msg = do
  time <- getZonedTime
  return $ formatTime defaultTimeLocale "[ %F %X%4Q ] " time ++ msg

-- | Construct logger using provided LogAction
logger :: String -> LogAction IO String -> IO ()
logger msg action =
  timestamp msg >>= unLogAction (logStringStdout <> action)

type Logger = String -> IO ()

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
downloadBroadcast :: Logger -> BroadcastLink -> FilePath -> IO ()
downloadBroadcast log (link@BroadcastLink {url, dj}) path = do
  download <- openURI $ "http://radio.fobby.net/archives/" ++ toString url
  case download of
    Left error -> log $ concat ["failed to download: ", error]
    Right body -> writeFileBS path body

-- | Parse out [BroadcastLink] from ArchiveAPI response for a given DJ
parseBroadcasts :: DJ -> ApiHTML -> IO [BroadcastLink]
parseBroadcasts dj html = do
  urls <-
    runX $
      readString [withParseHTML yes, withWarnings no] html
        >>> css ("a" :: String)
        >>> getAttrValue "href"
  return $ map (\url -> BroadcastLink {url = toText url, dj = coerce dj}) urls

-- | HTTP Client for ArchiveAPI
archiveClient :: RunClient m => ArchiveAPI (AsClientT m)
archiveClient = genericClient @ArchiveAPI

-- | Use archiveClient to fetch broadcast listings for a given DJ
fetchDJ :: Logger -> DJ -> IO (Maybe [BroadcastLink])
fetchDJ log dj = do
  log $ concat ["grabbing listings for ", show dj]
  archives <- runArchive $ listDJ archiveClient (Just dj)
  case archives of
    Left error -> return Nothing
    Right html -> Just <$> parseBroadcasts dj (toString html)

-- | SQLite table for BroadcastLink
broadcasts :: DB.Table BroadcastLink
broadcasts = DB.table "broadcasts" [#url :- DB.primary]

-- | Insert links to broadcasts into `broadcast` table
insertBroadcastLinks :: Logger -> FilePath -> [BroadcastLink] -> IO ()
insertBroadcastLinks log path input = do
  log $ concat ["adding broadcast links to database: ", path]
  result <- try @SeldaError $
    DBS.withSQLite path $ do
      DB.tryCreateTable broadcasts
      DB.tryInsert broadcasts input
  case result of
    Left error -> do
      log $ concat ["failed to add broadcast links: ", show error]
    otherwise -> return ()

downloadLinks :: Logger -> [BroadcastLink] -> IO ()
downloadLinks log links = forM_ links $ \(link@BroadcastLink {url}) -> do
  log $ concat ["dowloading ", show url]
  let path = "downloads/" ++ toString url
   in do
        createDirectoryIfMissing True $ takeDirectory path
        alreadyDownloaded <- doesFileExist path
        if not alreadyDownloaded
          then downloadBroadcast log link path
          else log $ concat ["already downloaded, skipping ", path]

-- | Fetch broadcasts listings for all known artists, add to database
main :: IO ()
main = do
  startTime <- getZonedTime
  let path = formatTime defaultTimeLocale "mystical-record-%F.log" startTime
      log = withLogStringFile path . logger
   in do
        broadcastLinks <- mconcat . catMaybes <$> traverse (fetchDJ log) knownDJs
        insertBroadcastLinks log "broadcasts.db" broadcastLinks
        downloadLinks log broadcastLinks
