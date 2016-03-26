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
import Data.List (delete, findIndex)
import Web.HttpApiData (parseQueryParamMaybe)

-- Setup a global in memory queue
queue :: TVar [QueueEntry]
queue = unsafePerformIO $ newTVarIO []

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- Not technically an error, but we want to treat it like one
err204 :: ServantErr
err204 = ServantErr { errHTTPCode = 204
                    , errReasonPhrase = "No Content"
                    , errBody = ""
                    , errHeaders = []
                    }

-- Stupid hack, not sure why this is necessary
instance FromText UTCTime where 
  fromText = parseQueryParamMaybe

type Handler = EitherT ServantErr IO

type API = "queue" :> Capture "id" Int :> ReqBody '[JSON] UTCTime :> Put '[JSON] ()
      :<|> "queue" :> Capture "id" Int :> Delete '[JSON] ()
      :<|> "queue" :> Capture "id" Int :> Get '[JSON] Position
      :<|> "queue" :> "pop" :> Post '[JSON] QueueEntry
      :<|> "queue" :> Get '[JSON] [Int]
      :<|> "queue" :> "wait" :> QueryParam "time" UTCTime :> Get '[JSON] AverageWait

server :: Server API
server = insertEntry
    :<|> deleteEntry
    :<|> findEntryPosition
    :<|> popEntry
    :<|> listEntries
    :<|> averageWaitTime

insertEntry :: Int -> UTCTime -> Handler ()
insertEntry i added = do
  now <- liftIO getCurrentTime
  let entry = QE i added
  result <- liftIO . atomically $ do
    q <- readTVar queue
    if entry `elem` q then
      return Nothing -- This is ugly, find a better way to do this
    else
      modifyTVar queue (\xs -> sortQueue now (entry:xs)) >> (return $ Just ())
  maybe (throwError err409) return result

deleteEntry :: Int -> Handler ()
deleteEntry i = liftIO . atomically $ modifyTVar queue (\q -> delete (QE i undefined) q)

findEntryPosition :: Int -> Handler Position
findEntryPosition i = do
  q <- getSortedQueue
  let maybePosition = findIndex (\(p,x) -> entryId x == i) $ zip [0..] q
  maybe (throwError err404) (return . Position) maybePosition

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

listEntries :: Handler [Int]
listEntries = do
  q <- getSortedQueue
  return $ map entryId q

averageWaitTime :: Maybe UTCTime -> Handler AverageWait
averageWaitTime Nothing = throwError err400
averageWaitTime (Just now) = do
  q <- liftIO . atomically $ readTVar queue
  let totalWait = foldl (\d e -> d + (now `diffUTCTime` (entryTime e))) (fromInteger 0) q
      averageWait = totalWait / (fromInteger . toInteger $ (max (length q) 1))
  return $ AW averageWait
  