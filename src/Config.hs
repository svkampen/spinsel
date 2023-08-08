{-# LANGUAGE BlockArguments, ImpredicativeTypes, LambdaCase, OverloadedStrings #-}
module Config (loadConfig) where

import Control.Arrow (left)
import Data.Text     (Text)
import Toml          (TomlCodec, (.=))

import Toml qualified

import Types (Config (..))

configCodec :: TomlCodec Config
configCodec = Config
    <$> Toml.string "layout-dir"                  .= configLayoutDirectory
    <*> Toml.string "output-dir"                  .= configOutputDirectory
    <*> Toml.string "posts-dir"                   .= configPostsDirectory
    <*> Toml.text "default-layout"                .= configDefaultLayout
    <*> Toml.arrayOf Toml._String "template-exts" .= configTemplateExts
    <*> Toml.text "name"                          .= configName

loadConfig :: IO (Either Text Config)
loadConfig = do
    res <- Toml.decodeFileEither configCodec "config.toml"
    return $ left Toml.prettyTomlDecodeErrors res
