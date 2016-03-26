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

type DocsAPI = "docs" :> Raw

apiDocs :: API
apiDocs = docs API.api

api :: Proxy DocsAPI
api = Proxy

server :: Server DocsAPI
server = serveDocs
  where serveDocs _ respond = respond $ responseLBS ok200 [plain] Docs.docsBS
        plain = ("Content-Type", "text/plain")

docsBS :: ByteString
docsBS = encodeUtf8 . pack . markdown $ docsWithIntros [intro] API.api
  where intro = DocIntro "Octarine Sapient Walnut (Aspect Homework)" ["Implements Aspect Homework in Haskell using Servant"]

instance ToSample Position where
  toSamples _ = singleSample (Position 12)

instance ToSample Int where
  toSamples _ = singleSample 42

instance ToSample UTCTime where
  toSamples _ = singleSample (unsafePerformIO getCurrentTime)

instance ToSample AverageWait where
  toSamples _ = singleSample (AW 60)

instance ToSample QueueEntry where
  toSamples _ = singleSample (QE 15 (unsafePerformIO getCurrentTime))

instance ToCapture (Capture "id" Int) where
  toCapture _ =
    DocCapture "id"
               "(integer) ID of queue entry"

instance ToParam (QueryParam "time" UTCTime) where
  toParam _ =
    DocQueryParam "time"
                  [show $ unsafePerformIO getCurrentTime]
                  "The current time."
                  Normal

