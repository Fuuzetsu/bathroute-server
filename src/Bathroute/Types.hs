{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
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

$(deriveJSON defaultOptions ''PublicKey)

-- | The standard ByteString instances in aeson are broken: they assume UTF8
-- encoding. This type instead uses base64 for proper round-tripping.
newtype ByteString64 = ByteString64 { unByteString64 ∷ ByteString }
                     deriving (Eq, Read, Show, Data,
                               Typeable, Ord, Hashable)

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
  toJSON (ByteString64 bs) = toJSON (B64.encode bs)

instance FromJSON ByteString64 where
  parseJSON o =
    parseJSON o >>= either fail (return . ByteString64) . B64.decode

-- | Actions one can request with a person.
data FriendAction = Add PublicKey | Remove PublicKey
                  | Block PublicKey | Share PublicKey
                  deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''FriendAction)

-- -- | When a user requests a new alias, we decode it to this data type which
-- -- we later verify the signature of.
-- data AliasRequest =
--   AliasRequest { aliasName ∷ String
--                  -- ^ The new name that the user wishes to use.
--                }
--   deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''AliasRequest)

-- | User status change.
data OnlineRequest = Online | Offline
                   deriving (Eq, Show)

$(deriveJSON defaultOptions ''OnlineRequest)

-- -- | Key of a person user wishes to interact with and an action describing
-- -- what they want to do.
-- data FriendRequest = FriendRequest { friendKey ∷ PublicKey
--                                    , friendAction ∷ FriendAction }

-- $(deriveJSON defaultOptions ''FriendRequest)
-- $(deriveJSON defaultOptions ''Request)

-- verify' ∷ Request ByteString
--         → Either (Either String RSAError) (Maybe ByteString64)
-- verify' req@(Request k r s m) = case B64.decode m of
--   Left e → Left $ Left e
--   Right s' → case verify k (LB.fromStrict m) (LB.fromStrict s') of
--     Left rsa → Left $ Right rsa
--     Right False → Right Nothing
--     Right True → Right $ Just m


data ServerComm = ServerComm { senderKey ∷ PublicKey
                             , senderSignature ∷ ByteString
                               -- ^ Signed message to the server itself
                               -- protecting recipient.
                             , serverMessage ∷ ByteString
                             }

data ServerMessage = ServerMessage { requestType ∷ RequestType
                                   , secretMessage ∷ ByteString
                                   , messageSignature ∷ ByteString
                                     -- ^ Signed contents of the message
                                   , messageRecipient ∷ PublicKey
                                   }

data RequestType = OnlineChange | FriendChange

$(derive makeBinary ''RequestType)
$(derive makeBinary ''OnlineRequest)
$(derive makeBinary ''FriendAction)

instance Binary ServerComm where
  put (ServerComm k s m) = put (k, s, m)
  get = liftM3 ServerComm get get get

instance Binary ServerMessage where
  put (ServerMessage t m s r) = put (t, m, s, r)
  get = liftM4 ServerMessage get get get get

type ServerKeys = (PublicKey, PrivateKey)

data Request = OnlineStatus OnlineRequest | FriendStatus FriendAction

-- | Main decoder which takes care of decrypting and deserialising messages
decodeMessage ∷ ServerKeys → ServerComm → Either String (PublicKey, Request)
decodeMessage (spub, spriv) (ServerComm sendpub sendsig servmsg) =
  -- First verify that thet the message server is getting is what's intended
  case verify sendpub (LB.fromStrict servmsg) (LB.fromStrict sendsig) of
    Left rsaerr → Left $ show rsaerr -- failed to decode properly
    Right False → Left "Failed to verify signature on serverMessage"
    Right True → case decrypt spriv (LB.fromStrict servmsg) of
      Left rsaerr → Left $ show rsaerr
      Right xs →
        let ServerMessage t ms sig r = B.decode xs
        in case verify sendpub (LB.fromStrict sig) (LB.fromStrict ms) of
          Left rsaerr → Left $ show rsaerr
          Right False → Left "Failed to verify signature on secretMessage"
          Right True
            | spub == r → case dec ms of
              Left rsaerr → Left $ show rsaerr
              Right bs → fmap (sendpub,) $ case t of
                OnlineChange → Right . OnlineStatus $ B.decode bs
                FriendChange → Right . FriendStatus $ B.decode bs
            | otherwise → Left "No handling of messages to others yet"

            where
              dec = decrypt spriv . LB.fromStrict
