{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Network
import qualified Data.Conduit.Text as CT
import           Data.Text (toUpper)


-- | When a user requests a new alias, we decode it to this data type which
-- we later verify the signature of.
data AliasRequest =
  AliasRequest { aliasName ∷ ByteString
                 -- ^ The new name that the user wishes to use.
               , aliasKey ∷ PublicKey
                 -- ^ The 'PublicKey' of the user.
               }

data OnlineRequest =
  OnlineRequest { onlineKey ∷ PublicKey
                  -- ^ Key of the person coming online.
                }

$(deriveJSON defaultOptions ''PublicKey)
