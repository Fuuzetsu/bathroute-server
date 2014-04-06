{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module      :  Bathroute.Types
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Types used throughout the server.
module Bathroute.Types where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as LB
import           Data.Map

-- | Each user is now just identified by an 'Integer'.
data User = User Integer deriving (Show, Eq)

-- | Alias for a 'Double' representing Latitude.
type Latitude = Double

-- | Alias for a 'Double' representing Longitude.
type Longitude = Double

-- | A location is just @('Latitude', 'Longitude')@.
type Location = (Latitude, Longitude)

-- | Each event has a 'Location' and a 'User' that created it.
data Event = Event Latitude Longitude User

-- | Each @Alias@ belongs to a 'User'.
data Alias = Alias String User

-- | A list of 'Friends' is just a 'Map' from a 'User' to multiple
-- 'User's he's currently friends with.
newtype Friends = Friends (Map User [User])

-- | Actions one can request with a person.
data FriendAction = Add User
                  | Remove User
                  | Block User
                  | Share User
                  deriving (Eq, Show)

-- | User status change.
data OnlineRequest = Online
                   | Offline
                   deriving (Eq, Show)

-- | Type used when a 'User' is initially asking for an 'Alias'. It
-- might be taken!
newtype AliasRequest = DesiredAlias String deriving (Show, Eq)


-- | Sum type gathering all the different requests we can expect to
-- get/send from/to clients.
data Request = OnlineStatus OnlineRequest
             | FriendStatus FriendAction
             | AliasStatus AliasRequest
               deriving (Show, Eq)


-- | A message to the server is nothing but a 'Request' along with
-- 'User' that the request is meant for. Server then decides who to
-- forward the message to.
data ServerMessage = ServerMessage { recipient ∷ User
                                   , request ∷ Request
                                   } deriving (Show, Eq)

-- | Handy type alias for an internal represantation of list of clients
-- that we store in our 'Server' 'Map'.
type UserVal = (Maybe User, LB.ByteString → STM ())

-- | Server datatype keeping track of who's connected, how to talk to
-- them and their 'User' IDs if any.
newtype Server = Server {
  _clients ∷ MVar (Map ThreadId UserVal)
  }

-- | A simple 'Status' data type that we may in the future use to
-- indicate success or failure of client requests.
data Status = OK | Failed String deriving (Show, Eq)

$(deriveJSON defaultOptions ''OnlineRequest)
$(deriveJSON defaultOptions ''AliasRequest)
$(deriveJSON defaultOptions ''FriendAction)
$(deriveJSON defaultOptions ''Request)
$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''ServerMessage)
$(deriveJSON defaultOptions ''Status)
