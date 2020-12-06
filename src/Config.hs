{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , load
  ) where

import Data.ByteString (ByteString)
import qualified Data.Text.IO as T (readFile)
import System.Directory (getHomeDirectory)
import qualified Toml (byteString, decode, string)
import Toml (TomlCodec, TomlDecodeError, (.=))

data Config = Config
  { harvestToken :: ByteString
  , harvestAccount :: ByteString
  , projectId :: String
  }

configCodec :: TomlCodec Config
configCodec =
  Config <$> Toml.byteString "harvest_token" .= harvestToken <*>
  Toml.byteString "harvest_account" .= harvestAccount <*>
  Toml.string "project_id" .= projectId

load :: IO (Either [TomlDecodeError] Config)
load = do
  h <- getHomeDirectory
  c <- T.readFile (h ++ "/.config/ttyme/ttyme.toml")
  pure (Toml.decode configCodec c)
