{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , insertEntry
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Time
import Control.Arrow hiding (app)
import Data.List (sortBy, partition)
import Control.Monad.STM
import Control.Monad.Trans.Either

data Type = Normal | Priority | VIP | Override
  deriving (Show,Eq)

data QueueEntry = QE 
  { entryId :: Int
  , entryTime :: UTCTime
  , entryType :: Type
  }

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "queue" :> Capture "id" Int :> Put '[JSON] ()
      :<|> "queue" :> Get '[JSON] Int

idToType :: Int -> Type
idToType x | x `mod` 15 == 0 = Override
           | x `mod` 5 == 0 = VIP
           | x `mod` 3 == 0 = Priority
           | otherwise = Normal

sortQueue :: UTCTime -> [QueueEntry] -> [QueueEntry]
sortQueue now xs = uncurry (++) $ (sortBy sort') *** (sortBy sort') $ partition (\x -> entryType x == Override) xs
  where
    scoreEntry x = calculateScore (entryType x) (min 0 (now `diffUTCTime` entryTime x))
    sort' x y = compare (scoreEntry x) (scoreEntry y)

calculateScore :: Type -> NominalDiffTime -> NominalDiffTime
calculateScore t d = 
  case t of 
    Override -> d
    Normal -> d
    Priority -> coerce $ max 3 ((coerce d) * log (coerce d))
    VIP -> coerce $ max 4 (2 * (coerce d) * log (coerce d))

coerce :: (Num a, Ord a, Real a, Fractional b, Num b) => a -> b
coerce = fromRational . toRational

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = insertEntry
    :<|> return 42

insertEntry :: Int -> EitherT ServantErr IO ()
insertEntry _ = return ()

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
