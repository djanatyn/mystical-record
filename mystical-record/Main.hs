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
import Data.Text
import Network.HTTP.Client
import Network.HTTP.Media hiding (Accept)
import Servant.API
import Servant.API.Generic
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic

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
    Right a -> print a
    Left error -> print error

-- >> runArchive $ listDJ archiveClient (Just $ DJ "umbra")
-- >> Right "<html>\n<head>\n\t<title>Radio PSI Archives</title>\n\t<style>\n\t\ta {color: #9a9d2f !important;}\n\t</style>\n</head>\n<body style=\"color:white;\">\n<a href='umbra/radiopsidumpdjumbra-2015_07_17.ogg'>radiopsidumpdjumbra-2015_07_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_07_24.ogg'>radiopsidumpdjumbra-2015_07_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_07_31.ogg'>radiopsidumpdjumbra-2015_07_31.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_08_07.ogg'>radiopsidumpdjumbra-2015_08_07.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_08_15.ogg'>radiopsidumpdjumbra-2015_08_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_08_21.ogg'>radiopsidumpdjumbra-2015_08_21.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_08_28.ogg'>radiopsidumpdjumbra-2015_08_28.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_09_04.ogg'>radiopsidumpdjumbra-2015_09_04.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_09_11.ogg'>radiopsidumpdjumbra-2015_09_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_09_18.ogg'>radiopsidumpdjumbra-2015_09_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_09_27.ogg'>radiopsidumpdjumbra-2015_09_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_10_04.ogg'>radiopsidumpdjumbra-2015_10_04.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_10_11.ogg'>radiopsidumpdjumbra-2015_10_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_10_18.ogg'>radiopsidumpdjumbra-2015_10_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_10_21.ogg'>radiopsidumpdjumbra-2015_10_21.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_10_25.ogg'>radiopsidumpdjumbra-2015_10_25.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_11_01.ogg'>radiopsidumpdjumbra-2015_11_01.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_11_08.ogg'>radiopsidumpdjumbra-2015_11_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_11_15.ogg'>radiopsidumpdjumbra-2015_11_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_11_22.ogg'>radiopsidumpdjumbra-2015_11_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_11_29.ogg'>radiopsidumpdjumbra-2015_11_29.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_12_06.ogg'>radiopsidumpdjumbra-2015_12_06.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_12_13.ogg'>radiopsidumpdjumbra-2015_12_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_12_20.ogg'>radiopsidumpdjumbra-2015_12_20.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2015_12_27.ogg'>radiopsidumpdjumbra-2015_12_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_01_03.ogg'>radiopsidumpdjumbra-2016_01_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_01_10.ogg'>radiopsidumpdjumbra-2016_01_10.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_01_17.ogg'>radiopsidumpdjumbra-2016_01_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_01_24.ogg'>radiopsidumpdjumbra-2016_01_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_01_31.ogg'>radiopsidumpdjumbra-2016_01_31.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_02_07.ogg'>radiopsidumpdjumbra-2016_02_07.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_02_14.ogg'>radiopsidumpdjumbra-2016_02_14.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_02_21.ogg'>radiopsidumpdjumbra-2016_02_21.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_02_27.ogg'>radiopsidumpdjumbra-2016_02_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_03_06.ogg'>radiopsidumpdjumbra-2016_03_06.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_03_12.ogg'>radiopsidumpdjumbra-2016_03_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_03_13.ogg'>radiopsidumpdjumbra-2016_03_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_03_20.ogg'>radiopsidumpdjumbra-2016_03_20.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_03_27.ogg'>radiopsidumpdjumbra-2016_03_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_04_03.ogg'>radiopsidumpdjumbra-2016_04_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_04_10.ogg'>radiopsidumpdjumbra-2016_04_10.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_04_17.ogg'>radiopsidumpdjumbra-2016_04_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_04_24.ogg'>radiopsidumpdjumbra-2016_04_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_05_01.ogg'>radiopsidumpdjumbra-2016_05_01.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_05_15.ogg'>radiopsidumpdjumbra-2016_05_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_05_22.ogg'>radiopsidumpdjumbra-2016_05_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_05_29.ogg'>radiopsidumpdjumbra-2016_05_29.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_06_12.ogg'>radiopsidumpdjumbra-2016_06_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_06_19.ogg'>radiopsidumpdjumbra-2016_06_19.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_06_24.ogg'>radiopsidumpdjumbra-2016_06_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_06_26.ogg'>radiopsidumpdjumbra-2016_06_26.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_07_03.ogg'>radiopsidumpdjumbra-2016_07_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_07_10.ogg'>radiopsidumpdjumbra-2016_07_10.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_07_17.ogg'>radiopsidumpdjumbra-2016_07_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_07_24.ogg'>radiopsidumpdjumbra-2016_07_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_07_31.ogg'>radiopsidumpdjumbra-2016_07_31.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_08_07.ogg'>radiopsidumpdjumbra-2016_08_07.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_08_14.ogg'>radiopsidumpdjumbra-2016_08_14.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_08_21.ogg'>radiopsidumpdjumbra-2016_08_21.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_08_27.ogg'>radiopsidumpdjumbra-2016_08_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_09_11.ogg'>radiopsidumpdjumbra-2016_09_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_09_18.ogg'>radiopsidumpdjumbra-2016_09_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_09_25.ogg'>radiopsidumpdjumbra-2016_09_25.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_10_02.ogg'>radiopsidumpdjumbra-2016_10_02.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_10_09.ogg'>radiopsidumpdjumbra-2016_10_09.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_10_16.ogg'>radiopsidumpdjumbra-2016_10_16.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_10_23.ogg'>radiopsidumpdjumbra-2016_10_23.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_10_30.ogg'>radiopsidumpdjumbra-2016_10_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_11_06.ogg'>radiopsidumpdjumbra-2016_11_06.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_11_13.ogg'>radiopsidumpdjumbra-2016_11_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_11_20.ogg'>radiopsidumpdjumbra-2016_11_20.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_11_27.ogg'>radiopsidumpdjumbra-2016_11_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_12_04.ogg'>radiopsidumpdjumbra-2016_12_04.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_12_11.ogg'>radiopsidumpdjumbra-2016_12_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_12_18.ogg'>radiopsidumpdjumbra-2016_12_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2016_12_25.ogg'>radiopsidumpdjumbra-2016_12_25.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_01_01.ogg'>radiopsidumpdjumbra-2017_01_01.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_01_08.ogg'>radiopsidumpdjumbra-2017_01_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_01_15.ogg'>radiopsidumpdjumbra-2017_01_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_01_22.ogg'>radiopsidumpdjumbra-2017_01_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_01_29.ogg'>radiopsidumpdjumbra-2017_01_29.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_02_05.ogg'>radiopsidumpdjumbra-2017_02_05.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_02_12.ogg'>radiopsidumpdjumbra-2017_02_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_02_19.ogg'>radiopsidumpdjumbra-2017_02_19.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_02_26.ogg'>radiopsidumpdjumbra-2017_02_26.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_03_05.ogg'>radiopsidumpdjumbra-2017_03_05.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_03_12.ogg'>radiopsidumpdjumbra-2017_03_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_03_19.ogg'>radiopsidumpdjumbra-2017_03_19.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_03_26.ogg'>radiopsidumpdjumbra-2017_03_26.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_04_02.ogg'>radiopsidumpdjumbra-2017_04_02.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_04_09.ogg'>radiopsidumpdjumbra-2017_04_09.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_04_16.ogg'>radiopsidumpdjumbra-2017_04_16.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_04_23.ogg'>radiopsidumpdjumbra-2017_04_23.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_04_30.ogg'>radiopsidumpdjumbra-2017_04_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_05_07.ogg'>radiopsidumpdjumbra-2017_05_07.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_05_14.ogg'>radiopsidumpdjumbra-2017_05_14.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_05_28.ogg'>radiopsidumpdjumbra-2017_05_28.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_06_04.ogg'>radiopsidumpdjumbra-2017_06_04.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_06_11.ogg'>radiopsidumpdjumbra-2017_06_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_06_18.ogg'>radiopsidumpdjumbra-2017_06_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_07_02.ogg'>radiopsidumpdjumbra-2017_07_02.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_07_09.ogg'>radiopsidumpdjumbra-2017_07_09.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_07_16.ogg'>radiopsidumpdjumbra-2017_07_16.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_07_23.ogg'>radiopsidumpdjumbra-2017_07_23.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_07_30.ogg'>radiopsidumpdjumbra-2017_07_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_08_06.ogg'>radiopsidumpdjumbra-2017_08_06.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_08_13.ogg'>radiopsidumpdjumbra-2017_08_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_08_20.ogg'>radiopsidumpdjumbra-2017_08_20.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_08_27.ogg'>radiopsidumpdjumbra-2017_08_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_09_03.ogg'>radiopsidumpdjumbra-2017_09_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_09_10.ogg'>radiopsidumpdjumbra-2017_09_10.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_09_17.ogg'>radiopsidumpdjumbra-2017_09_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_09_24.ogg'>radiopsidumpdjumbra-2017_09_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_09_30.ogg'>radiopsidumpdjumbra-2017_09_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_10_08.ogg'>radiopsidumpdjumbra-2017_10_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_10_15.ogg'>radiopsidumpdjumbra-2017_10_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_10_22.ogg'>radiopsidumpdjumbra-2017_10_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_10_28.ogg'>radiopsidumpdjumbra-2017_10_28.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_11_05.ogg'>radiopsidumpdjumbra-2017_11_05.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_11_12.ogg'>radiopsidumpdjumbra-2017_11_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_11_19.ogg'>radiopsidumpdjumbra-2017_11_19.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_11_26.ogg'>radiopsidumpdjumbra-2017_11_26.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_12_03.ogg'>radiopsidumpdjumbra-2017_12_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_12_10.ogg'>radiopsidumpdjumbra-2017_12_10.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_12_17.ogg'>radiopsidumpdjumbra-2017_12_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2017_12_30.ogg'>radiopsidumpdjumbra-2017_12_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_01_21.ogg'>radiopsidumpdjumbra-2018_01_21.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_01_28.ogg'>radiopsidumpdjumbra-2018_01_28.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_02_11.ogg'>radiopsidumpdjumbra-2018_02_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_02_18.ogg'>radiopsidumpdjumbra-2018_02_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_02_25.ogg'>radiopsidumpdjumbra-2018_02_25.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_03_04.ogg'>radiopsidumpdjumbra-2018_03_04.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_03_11.ogg'>radiopsidumpdjumbra-2018_03_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_03_25.ogg'>radiopsidumpdjumbra-2018_03_25.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_04_01.ogg'>radiopsidumpdjumbra-2018_04_01.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_04_08.ogg'>radiopsidumpdjumbra-2018_04_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_04_22.ogg'>radiopsidumpdjumbra-2018_04_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_04_29.ogg'>radiopsidumpdjumbra-2018_04_29.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_05_06.ogg'>radiopsidumpdjumbra-2018_05_06.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_05_13.ogg'>radiopsidumpdjumbra-2018_05_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_06_10.ogg'>radiopsidumpdjumbra-2018_06_10.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_06_17.ogg'>radiopsidumpdjumbra-2018_06_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_07_01.ogg'>radiopsidumpdjumbra-2018_07_01.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_07_08.ogg'>radiopsidumpdjumbra-2018_07_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_07_15.ogg'>radiopsidumpdjumbra-2018_07_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_07_22.ogg'>radiopsidumpdjumbra-2018_07_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_08_05.ogg'>radiopsidumpdjumbra-2018_08_05.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_08_12.ogg'>radiopsidumpdjumbra-2018_08_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_08_19.ogg'>radiopsidumpdjumbra-2018_08_19.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_09_02.ogg'>radiopsidumpdjumbra-2018_09_02.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_09_16.ogg'>radiopsidumpdjumbra-2018_09_16.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_09_30.ogg'>radiopsidumpdjumbra-2018_09_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_10_07.ogg'>radiopsidumpdjumbra-2018_10_07.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_10_14.ogg'>radiopsidumpdjumbra-2018_10_14.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_10_28.ogg'>radiopsidumpdjumbra-2018_10_28.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_11_04.ogg'>radiopsidumpdjumbra-2018_11_04.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_11_11.ogg'>radiopsidumpdjumbra-2018_11_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_11_18.ogg'>radiopsidumpdjumbra-2018_11_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_11_25.ogg'>radiopsidumpdjumbra-2018_11_25.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_12_02.ogg'>radiopsidumpdjumbra-2018_12_02.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_12_09.ogg'>radiopsidumpdjumbra-2018_12_09.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_12_16.ogg'>radiopsidumpdjumbra-2018_12_16.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_12_23.ogg'>radiopsidumpdjumbra-2018_12_23.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2018_12_30.ogg'>radiopsidumpdjumbra-2018_12_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_01_13.ogg'>radiopsidumpdjumbra-2019_01_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_01_20.ogg'>radiopsidumpdjumbra-2019_01_20.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_01_27.ogg'>radiopsidumpdjumbra-2019_01_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_02_03.ogg'>radiopsidumpdjumbra-2019_02_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_02_10.ogg'>radiopsidumpdjumbra-2019_02_10.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_02_17.ogg'>radiopsidumpdjumbra-2019_02_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_02_24.ogg'>radiopsidumpdjumbra-2019_02_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_03_03.ogg'>radiopsidumpdjumbra-2019_03_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_03_10.ogg'>radiopsidumpdjumbra-2019_03_10.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_03_17.ogg'>radiopsidumpdjumbra-2019_03_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_03_24.ogg'>radiopsidumpdjumbra-2019_03_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_03_31.ogg'>radiopsidumpdjumbra-2019_03_31.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_04_07.ogg'>radiopsidumpdjumbra-2019_04_07.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_04_14.ogg'>radiopsidumpdjumbra-2019_04_14.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_04_21.ogg'>radiopsidumpdjumbra-2019_04_21.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_04_28.ogg'>radiopsidumpdjumbra-2019_04_28.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_05_05.ogg'>radiopsidumpdjumbra-2019_05_05.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_05_12.ogg'>radiopsidumpdjumbra-2019_05_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_05_19.ogg'>radiopsidumpdjumbra-2019_05_19.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_05_26.ogg'>radiopsidumpdjumbra-2019_05_26.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_06_02.ogg'>radiopsidumpdjumbra-2019_06_02.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_06_16.ogg'>radiopsidumpdjumbra-2019_06_16.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_06_23.ogg'>radiopsidumpdjumbra-2019_06_23.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_07_07.ogg'>radiopsidumpdjumbra-2019_07_07.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_07_14.ogg'>radiopsidumpdjumbra-2019_07_14.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_07_28.ogg'>radiopsidumpdjumbra-2019_07_28.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_08_04.ogg'>radiopsidumpdjumbra-2019_08_04.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_08_18.ogg'>radiopsidumpdjumbra-2019_08_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_08_25.ogg'>radiopsidumpdjumbra-2019_08_25.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_09_08.ogg'>radiopsidumpdjumbra-2019_09_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_09_15.ogg'>radiopsidumpdjumbra-2019_09_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_09_22.ogg'>radiopsidumpdjumbra-2019_09_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_09_29.ogg'>radiopsidumpdjumbra-2019_09_29.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_10_06.ogg'>radiopsidumpdjumbra-2019_10_06.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_10_13.ogg'>radiopsidumpdjumbra-2019_10_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_10_20.ogg'>radiopsidumpdjumbra-2019_10_20.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_10_27.ogg'>radiopsidumpdjumbra-2019_10_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_11_03.ogg'>radiopsidumpdjumbra-2019_11_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_11_17.ogg'>radiopsidumpdjumbra-2019_11_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_11_24.ogg'>radiopsidumpdjumbra-2019_11_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_12_01.ogg'>radiopsidumpdjumbra-2019_12_01.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_12_08.ogg'>radiopsidumpdjumbra-2019_12_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_12_15.ogg'>radiopsidumpdjumbra-2019_12_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_12_22.ogg'>radiopsidumpdjumbra-2019_12_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2019_12_29.ogg'>radiopsidumpdjumbra-2019_12_29.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_01_05.ogg'>radiopsidumpdjumbra-2020_01_05.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_01_12.ogg'>radiopsidumpdjumbra-2020_01_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_01_26.ogg'>radiopsidumpdjumbra-2020_01_26.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_02_02.ogg'>radiopsidumpdjumbra-2020_02_02.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_02_09.ogg'>radiopsidumpdjumbra-2020_02_09.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_02_16.ogg'>radiopsidumpdjumbra-2020_02_16.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_02_23.ogg'>radiopsidumpdjumbra-2020_02_23.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_03_01.ogg'>radiopsidumpdjumbra-2020_03_01.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_03_08.ogg'>radiopsidumpdjumbra-2020_03_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_03_15.ogg'>radiopsidumpdjumbra-2020_03_15.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_03_22.ogg'>radiopsidumpdjumbra-2020_03_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_03_29.ogg'>radiopsidumpdjumbra-2020_03_29.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_04_05.ogg'>radiopsidumpdjumbra-2020_04_05.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_04_12.ogg'>radiopsidumpdjumbra-2020_04_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_04_19.ogg'>radiopsidumpdjumbra-2020_04_19.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_04_26.ogg'>radiopsidumpdjumbra-2020_04_26.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_05_03.ogg'>radiopsidumpdjumbra-2020_05_03.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_05_17.ogg'>radiopsidumpdjumbra-2020_05_17.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_05_24.ogg'>radiopsidumpdjumbra-2020_05_24.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_05_30.ogg'>radiopsidumpdjumbra-2020_05_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_06_14.ogg'>radiopsidumpdjumbra-2020_06_14.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_06_21.ogg'>radiopsidumpdjumbra-2020_06_21.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_06_28.ogg'>radiopsidumpdjumbra-2020_06_28.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_07_05.ogg'>radiopsidumpdjumbra-2020_07_05.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_07_12.ogg'>radiopsidumpdjumbra-2020_07_12.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_07_19.ogg'>radiopsidumpdjumbra-2020_07_19.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_07_26.ogg'>radiopsidumpdjumbra-2020_07_26.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_08_02.ogg'>radiopsidumpdjumbra-2020_08_02.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_08_09.ogg'>radiopsidumpdjumbra-2020_08_09.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_08_16.ogg'>radiopsidumpdjumbra-2020_08_16.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_08_23.ogg'>radiopsidumpdjumbra-2020_08_23.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_08_30.ogg'>radiopsidumpdjumbra-2020_08_30.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_09_06.ogg'>radiopsidumpdjumbra-2020_09_06.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_09_13.ogg'>radiopsidumpdjumbra-2020_09_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_09_20.ogg'>radiopsidumpdjumbra-2020_09_20.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_09_27.ogg'>radiopsidumpdjumbra-2020_09_27.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_10_04.ogg'>radiopsidumpdjumbra-2020_10_04.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_10_11.ogg'>radiopsidumpdjumbra-2020_10_11.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_10_18.ogg'>radiopsidumpdjumbra-2020_10_18.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_10_25.ogg'>radiopsidumpdjumbra-2020_10_25.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_11_08.ogg'>radiopsidumpdjumbra-2020_11_08.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_11_22.ogg'>radiopsidumpdjumbra-2020_11_22.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_11_29.ogg'>radiopsidumpdjumbra-2020_11_29.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_12_06.ogg'>radiopsidumpdjumbra-2020_12_06.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_12_13.ogg'>radiopsidumpdjumbra-2020_12_13.ogg</a><br /><a href='umbra/radiopsidumpdjumbra-2020_12_20.ogg'>radiopsidumpdjumbra-2020_12_20.ogg</a><br /></body>\n"
