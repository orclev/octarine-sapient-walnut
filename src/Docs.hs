{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Docs
    ( apiDocs
    , docsBS
    , DocsAPI(..)
    , api
    , server
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Text.Lazy (pack)
import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server
import WorkQueue hiding (Type(..))
import qualified API as API
import Data.Time
import System.IO.Unsafe (unsafePerformIO)
import Network.HTTP.Types
import Data.Default (def)
import Text.Pandoc

type DocsAPI = "docs" :> Raw

apiDocs :: API
apiDocs = docs API.api

api :: Proxy DocsAPI
api = Proxy

server :: Server DocsAPI
server = serveDocs
  where serveDocs _ respond = respond $ responseLBS ok200 [html] Docs.docsBS
        html = ("Content-Type", "text/html")

docsBS :: ByteString
docsBS = encodeUtf8 . pack $ writeHtmlString writerOpts pandoc
  where 
    intro = DocIntro "Octarine Sapient Walnut (Aspect Homework)" ["Implements Aspect Homework in Haskell using Servant"]
    pandoc = extractRight . readMarkdown def . markdown $ docsWithIntros [intro] API.api
    extractRight (Right x) = x
    writerOpts = def { writerStandalone = False
                     , writerHtml5 = True 
                     }

instance ToSample Position where
  toSamples _ = [("Entries position in the queue, 0 indexed", Position 12)]

instance ToSample Int where
  toSamples _ = [("Management Override ID", 15),("Normal ID", 1),("Priority ID", 3),("VIP ID", 5)]

instance ToSample UTCTime where
  toSamples _ = [("Time in ISO 8601 format", unsafePerformIO getCurrentTime)]

instance ToSample AverageWait where
  toSamples _ = [("Average time entries have been in the queue", AW 60)]

instance ToSample QueueEntry where
  toSamples _ = [("Queue Entry" , QE 15 (unsafePerformIO getCurrentTime))]

instance ToCapture (Capture "id" Int) where
  toCapture _ =
    DocCapture "id"
               "(integer) ID of queue entry"

instance ToParam (QueryParam "time" UTCTime) where
  toParam _ =
    DocQueryParam "time"
                  [show $ unsafePerformIO getCurrentTime]
                  "The current time in ISO 8601 format"
                  Normal

