{-# LANGUAGE TemplateHaskell #-}
module WorkQueue 
    ( Type(..)
    , QueueEntry(..)
    , sortQueue
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Time
import Control.Arrow hiding (app)
import Data.List (sortBy, partition)

data Type = Normal | Priority | VIP | Override
  deriving (Show,Eq)

data QueueEntry = QE
  { entryId :: Int
  , entryTime :: UTCTime
  } deriving (Show)

instance Eq QueueEntry where
  a == b = entryId a == entryId b

$(deriveJSON defaultOptions ''QueueEntry)

idToType :: Int -> Type
idToType x | x `mod` 15 == 0 = Override
           | x `mod` 5 == 0 = VIP
           | x `mod` 3 == 0 = Priority
           | otherwise = Normal

sortQueue :: UTCTime -> [QueueEntry] -> [QueueEntry]
sortQueue now xs = uncurry (++) $ (sortBy sort') *** (sortBy sort') $ partition (\x -> (idToType $ entryId x) == Override) xs
  where
    scoreEntry x = calculateScore (idToType $ entryId x) (min 0 (now `diffUTCTime` entryTime x))
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

--data User = User
--  { userId        :: Int
--  , userFirstName :: String
--  , userLastName  :: String
--  } deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''User)

