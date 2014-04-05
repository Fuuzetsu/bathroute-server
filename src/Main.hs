{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Main
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Entry point for the Bathroute server.
module Main where

import           Control.Applicative
import           Codec.Crypto.RSA.Pure
import           Control.Monad.IO.Class (liftIO)
import           Crypto.Random
import qualified Data.Aeson as A
import           Data.Aeson.TH
import           Data.Binary
import           Data.ByteString
import           Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Conduit.Network
import qualified Data.Conduit.Text as CT
import           Data.Text (toUpper)

serverS ∷ Monad m ⇒ ServerSettings m
serverS = serverSettings 7777 HostAny

conduit ∷ MonadResource m ⇒
          ConduitM ByteString ByteString m ()
conduit = awaitForever handler
  where
    handler ∷ MonadResource m ⇒
              ByteString
            → ConduitM ByteString ByteString m ()
    handler b = do
      liftIO . Prelude.putStrLn . show $ (decode $ fromStrict b ∷ PublicKey)
      yield "Got it."

appD ∷ MonadResource m ⇒ Application m
appD a = src $$ conduit =$ snk
  where
    src = appSource a
    snk = appSink a

main ∷ IO ()
main = runResourceT $ runTCPServer serverS appD

runClient ∷ IO ()
runClient = do
  g ← newGenIO
  runResourceT $
    runTCPClient (clientSettings 7777 "127.0.0.1") (client g)

client ∷ MonadResource m ⇒ SystemRandom → Application m
client sr a = src $$ conduit' =$ snk
  where
    conduit' = do
      case generateKeyPair sr 100 of
        Left e → liftIO $ print e
        Right (pub, _priv, _) → do
          liftIO . Prelude.putStrLn $ "Sending " ++ show pub
          yield . toStrict $ encode pub
--      yield "quit"

    _msg m = yield m >> await >>= liftIO . print
    src = appSource a
    snk = appSink a
