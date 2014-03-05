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
               , aliasSignature ∷ ByteString
                 -- ^ The signed 'aliasName' that has to 'verify' using
                 -- the 'aliasKey'. Given a user's PublicKey and the signed
                 -- alias, we can be fairly confident that the alias request
                 -- comes from owner of the key. This stops anyone from
                 -- requesting aliases for other users.
               }
