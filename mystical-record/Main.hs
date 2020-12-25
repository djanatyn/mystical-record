{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Text
import Network.HTTP.Client
import Servant.API
import Servant.API.Generic
import Servant.Client
import Servant.Client.Core
import Servant.Client.Generic

newtype DJ = DJ Text deriving (Show, ToHttpApiData)

data ArchiveAPI r = ArchiveAPI
  { listDJ ::
      r :- "archives.php" :> QueryParam "dj" DJ
        :> Get '[PlainText] Text
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
main = return ()
