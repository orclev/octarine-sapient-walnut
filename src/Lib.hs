{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , insertEntry
    ) where

import Data.Aeson
--import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Time
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Either
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import System.IO.Unsafe (unsafePerformIO)
import WorkQueue

-- Setup a global in memory queue
queue :: TVar [QueueEntry]
queue = unsafePerformIO $ newTVarIO []

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

type Handler = EitherT ServantErr IO

type API = "queue" :> Capture "id" Int :> ReqBody '[JSON] UTCTime :> Put '[JSON] ()
      :<|> "queue" :> "pop" :> Post '[JSON] QueueEntry
      :<|> "queue" :> Get '[JSON] [Int]

server :: Server API
server = insertEntry
    :<|> popEntry
    :<|> listEntries

insertEntry :: Int -> UTCTime -> Handler ()
insertEntry i added = do
  now <- liftIO getCurrentTime
  let entry = QE i added
  liftIO . atomically $ modifyTVar queue (\xs -> sortQueue now (entry:xs))

getSortedQueue :: Handler [QueueEntry]
getSortedQueue = do
  now <- liftIO getCurrentTime
  q <- liftIO . atomically $ readTVar queue 
  return $ sortQueue now q

popEntry :: Handler QueueEntry
popEntry = do
  now <- liftIO getCurrentTime
  maybeEntry <- findEntry now
  maybe (throwError err204) return maybeEntry
  where
    findEntry t = liftIO . atomically $ do
      q <- readTVar queue
      let q' = sortQueue t q
      case q' of
        [] -> return Nothing
        x:_ -> do
          writeTVar queue (tail q')
          return $ Just $ head q'

-- Not technically an error, but we want to treat it like one
err204 :: ServantErr
err204 = ServantErr { errHTTPCode = 204
                    , errReasonPhrase = "No Content"
                    , errBody = ""
                    , errHeaders = []
                    }

listEntries :: Handler [Int]
listEntries = do
  q <- getSortedQueue
  return $ map entryId q
