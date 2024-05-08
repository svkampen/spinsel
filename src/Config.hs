{-# LANGUAGE BlockArguments, ImpredicativeTypes, OverloadedStrings, ScopedTypeVariables #-}

module Config (getConfig) where

import Data.Default
import Data.HashMap.Lazy   qualified as HM
import Data.List.Split     (splitOn)
import Data.String         (IsString)
import Data.Text           (Text)
import Data.Text           qualified as T
import Data.Text.IO        qualified as TIO
import Data.Vector         qualified as V
import Options.Applicative
import Text.Megaparsec     (errorBundlePretty)
import Text.Toml           (Node (..), parseTomlDoc)
import Types               (Config (..), ConfigOpt (..))
import Utils               (orElse)

commaSeparated :: ReadM (Maybe [String])
commaSeparated = Just . splitOn "," <$> str

maybeStr :: IsString s => ReadM (Maybe s)
maybeStr = Just <$> str

maybeStrOption :: forall s. IsString s => Mod OptionFields (Maybe s) -> Parser (Maybe s)
maybeStrOption opts = option maybeStr (opts <> value (Nothing :: Maybe s))

parseConfig' :: Parser ConfigOpt
parseConfig' =
  ConfigOpt
    <$> maybeStrOption (long "layout-dir" <> metavar "LAYOUTDIR" <> help "The directory storing layout files.")
    <*> maybeStrOption (long "output-dir" <> metavar "OUTPUTDIR" <> help "The directory that will contain the generated website.")
    <*> maybeStrOption (long "posts-dir" <> metavar "POSTSDIR" <> help "The directory storing posts.")
    <*> maybeStrOption (long "default-layout" <> metavar "LAYOUT" <> help "The layout that is applied to pages by default.")
    <*> option commaSeparated (long "layout-exts" <> metavar "LEXTS" <> help "File extensions for layouts" <> value Nothing)
    <*> option commaSeparated (long "page-exts" <> metavar "PEXTS" <> help "File extensions for pages" <> value Nothing)
    <*> maybeStrOption (long "site-url" <> metavar "SITEURL" <> help "Site URL")
    <*> maybeStrOption (long "name" <> metavar "NAME" <> help "Site name")
    <*> flag Nothing (Just True) (long "render-drafts" <> help "Render drafts?")

parseCliConfig :: ParserInfo ConfigOpt
parseCliConfig = info parseConfig' mempty

parseStrArr :: Node -> Maybe [String]
parseStrArr (VArray arr) =
  let nodes = V.toList arr
      strings = extractString <$> nodes
   in sequence strings
  where extractString :: Node -> Maybe String
        extractString n = case n of
          VString s -> Just . T.unpack $ s
          _         -> Nothing
parseStrArr _ = Nothing

unVString :: Node -> Maybe Text
unVString (VString s) = Just s
unVString _           = Nothing

unVBool :: Node -> Maybe Bool
unVBool (VBoolean b) = Just b
unVBool _            = Nothing

loadFileConfig :: IO (Either Text ConfigOpt)
loadFileConfig = do
  configContents <- TIO.readFile "config.toml"
  pure case parseTomlDoc "config.toml" configContents of
    Left e    -> Left . T.pack . errorBundlePretty $ e
    Right tbl -> Right ConfigOpt {
      configLayoutDirectoryOpt = T.unpack <$> (unVString =<< HM.lookup "layout-dir" tbl),
      configOutputDirectoryOpt = T.unpack <$> (unVString =<< HM.lookup "output-dir" tbl),
      configPostsDirectoryOpt = T.unpack <$> (unVString =<< HM.lookup "posts-dir" tbl),
      configDefaultLayoutOpt = unVString =<< HM.lookup "default-layout" tbl,
      configLayoutExtsOpt = parseStrArr =<< HM.lookup "layout-exts" tbl,
      configPageExtsOpt = parseStrArr =<< HM.lookup "page-exts" tbl,
      configNameOpt = unVString =<< HM.lookup "name" tbl,
      configSiteUrlOpt = unVString =<< HM.lookup "site-url" tbl,
      configRenderDraftsOpt = unVBool =<< HM.lookup "render-drafts" tbl
    }

getConfig :: IO (Either Text Config)
getConfig = do
  cliConfig <- execParser parseCliConfig
  Right fileConfig <- loadFileConfig
  pure . Right $ Config {
    configLayoutDirectory = (configLayoutDirectoryOpt cliConfig <|> configLayoutDirectoryOpt fileConfig) `orElse` configLayoutDirectory def,
    configOutputDirectory = (configOutputDirectoryOpt cliConfig <|> configOutputDirectoryOpt fileConfig) `orElse` configOutputDirectory def,
    configPostsDirectory = (configPostsDirectoryOpt cliConfig <|> configPostsDirectoryOpt fileConfig) `orElse` configPostsDirectory def,
    configDefaultLayout = (configDefaultLayoutOpt cliConfig <|> configDefaultLayoutOpt fileConfig) `orElse` configDefaultLayout def,
    configLayoutExts = (configLayoutExtsOpt cliConfig <|> configLayoutExtsOpt fileConfig) `orElse` configLayoutExts def,
    configPageExts = (configPageExtsOpt cliConfig <|> configPageExtsOpt fileConfig) `orElse` configPageExts def,
    configName = (configNameOpt cliConfig <|> configNameOpt fileConfig) `orElse` configName def,
    configSiteUrl = (configSiteUrlOpt cliConfig <|> configSiteUrlOpt fileConfig) `orElse` configSiteUrl def,
    configRenderDrafts = (configRenderDraftsOpt cliConfig <|> configRenderDraftsOpt fileConfig) `orElse` configRenderDrafts def
  }
