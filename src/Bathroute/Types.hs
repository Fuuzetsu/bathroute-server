{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import           Control.Applicative
import           Codec.Crypto.RSA.Pure
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Binary (get, put, Binary, putWord8, getWord8)
import qualified Data.Binary as B
import           Data.ByteString
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as U8
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Network
import qualified Data.Conduit.Text as CT
import           Data.Data
import           Data.DeriveTH
import           Data.Foldable hiding (mapM_)
import           Data.Hashable
import           Data.Text
import qualified Data.Text.Encoding as TE
import           Data.Traversable
import           Data.Typeable
-- import Database.Persist
import Database.Persist.TH
import qualified Database.Persist.Sqlite as S
import Database.Persist.Quasi

$(deriveJSON defaultOptions ''PublicKey)

-- | The standard ByteString instances in aeson are broken: they assume UTF8
-- encoding. This type instead uses base64 for proper round-tripping.
newtype ByteString64 = ByteString64 { unByteString64 ∷ ByteString }
                     deriving (Eq, Read, Show, Data,
                               Typeable, Ord, Hashable)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    publicKey ByteString
    deriving Eq Show
  Event
    latitude Double
    longitude Double
    description String
    creatorid UserId
    deriving Eq Show
  Alias
    name String
    ownerKey ByteString
    deriving Eq Show
  PendingFriends
    adder UserId
    addee UserId
    deriving Eq Show
  Friends
    listOwner UserId
    friendsWith [UserId]
    deriving Eq Show
|]

-- | Lazy ByteString to Text
strict :: LB.ByteString -> Text
strict = TE.decodeUtf8 . BS.concat . LB.toChunks
{-# INLINE strict #-}

-- | Text to lazy ByteString
lazy :: Text -> LB.ByteString
lazy = LB.fromChunks . (:[]) . TE.encodeUtf8
{-# INLINE lazy #-}

instance ToJSON BS.ByteString where
    toJSON = String . TE.decodeUtf8
    {-# INLINE toJSON #-}

instance FromJSON BS.ByteString where
    parseJSON = withText "ByteString" $ pure . TE.encodeUtf8
    {-# INLINE parseJSON #-}

instance ToJSON LB.ByteString where
    toJSON = toJSON . strict
    {-# INLINE toJSON #-}

instance FromJSON LB.ByteString where
    parseJSON = withText "Lazy ByteString" $ pure . lazy
    {-# INLINE parseJSON #-}

instance ToJSON ByteString64 where
  toJSON (ByteString64 bps) = toJSON (B64.encode bps)

instance FromJSON ByteString64 where
  parseJSON o =
    parseJSON o >>= either fail (return . ByteString64) . B64.decode

-- | Actions one can request with a person.
data FriendAction = Add PublicKey | Remove PublicKey
                  | Block PublicKey | Share PublicKey
                  deriving (Eq, Show)

-- | User status change.
data OnlineRequest = Online | Offline
                   deriving (Eq, Show)

data AliasRequest = DesiredAlias String

$(deriveJSON defaultOptions ''OnlineRequest)
$(deriveJSON defaultOptions ''AliasRequest)


data Request = OnlineStatus OnlineRequest
             | FriendStatus FriendAction
             | AliasStatus AliasRequest


data ServerComm = ServerComm { senderKey ∷ PublicKey
                             , senderSignature ∷ ByteString
                               -- ^ Signed message to the server itself
                               -- protecting recipient.
                             , serverMessage ∷ ByteString
                             }

data RequestType = OnlineChange | FriendChange

data ServerMessage = ServerMessage { requestType ∷ RequestType
                                   , secretMessage ∷ ByteString
                                   , messageSignature ∷ ByteString
                                     -- ^ Signed contents of the message
                                   , messageRecipient ∷ PublicKey
                                   }

$(derive makeBinary ''RequestType)
$(derive makeBinary ''AliasRequest)
$(derive makeBinary ''Request)
$(derive makeBinary ''OnlineRequest)
$(derive makeBinary ''FriendAction)

instance Binary ServerComm where
  put (ServerComm k s m) = put (k, s, m)
  get = liftM3 ServerComm get get get

instance Binary ServerMessage where
  put (ServerMessage t m s r) = put (t, m, s, r)
  get = liftM4 ServerMessage get get get get

$(deriveJSON defaultOptions ''ServerComm)

type ServerKeys = (PublicKey, PrivateKey)
data Status = OK | Failed String deriving (Show, Eq)

$(derive makeBinary ''Status)
$(deriveJSON defaultOptions ''Status)
