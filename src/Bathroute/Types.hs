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
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
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
import           Data.Foldable
import           Data.Hashable
import           Data.Text
import qualified Data.Text.Encoding as TE
import           Data.Traversable
import           Data.Typeable


-- | The standard ByteString instances in aeson are broken: they assume UTF8
-- encoding. This type instead uses base64 for proper round-tripping.
newtype ByteString64 = ByteString64 { unByteString64 :: ByteString }
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

-- | A 'Request' is a pair of
data Request a = Request PublicKey a
               deriving (Functor, Foldable, Traversable, Eq, Show)

-- | When a user requests a new alias, we decode it to this data type which
-- we later verify the signature of.
data AliasRequest =
  AliasRequest { aliasName ∷ String
                 -- ^ The new name that the user wishes to use.
               }
  deriving (Eq, Show)

data OnlineRequest = OnlineRequest
                   deriving (Eq, Show)

$(deriveJSON defaultOptions ''PublicKey)
