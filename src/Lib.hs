{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    ) where

import qualified API
import qualified Docs
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server
import Network.HTTP.Types

type API = API.API :<|> Docs.DocsAPI

api :: Proxy API
api = Proxy

server :: Server API
server = API.server :<|> Docs.server

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server