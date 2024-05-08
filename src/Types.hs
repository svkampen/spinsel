{-# LANGUAGE AllowAmbiguousTypes, BlockArguments, DataKinds, ExplicitForAll, OverloadedStrings,
             PatternSynonyms, StandaloneDeriving, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}

module Types
  ( Config (..),
    ConfigOpt (..),
    wrap,
    Spinsel,
    StatelessSpinsel,
    SpinselState (..),
    runSpinsel,
    runStatelessSpinsel,
    withSpinselState,
    Page (Page, filename, title, published, content, layout, targetPath, isDraft, ..),
    PageState (..),
    ContentType,
    TemplateName,
    module Control.Monad.Reader,
    module Control.Monad.State,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Default
import Data.Text            (Text)
import Data.Time            (Day)
import Text.Ginger          (SourcePos, Template)
import THUtils              (mkOptional)

data PageState = Raw | Rendered | Compiled | Templated

type family ContentType (a :: PageState) where
  ContentType 'Raw = Text
  ContentType 'Rendered = Text
  ContentType 'Compiled = Template SourcePos
  ContentType 'Templated = Text

type Layout = Text
type TemplateName = Text

data Page' c =
  Page'
  { _filename   :: FilePath,
    _title      :: Text,
    _published  :: Day,
    _layout     :: Layout,
    _content    :: c,
    _targetPath :: Maybe FilePath,
    _isDraft    :: Bool
  } deriving (Show, Eq)

newtype Page (s :: PageState) = P (Page' (ContentType s))

deriving instance Show (ContentType s) => Show (Page s)
deriving instance Eq (ContentType s) => Eq (Page s)

pattern Page :: FilePath -> Text -> Day -> Layout -> ContentType s -> Maybe FilePath -> Bool -> Page s
pattern Page{filename, title, published, layout, content, targetPath, isDraft} = P (Page' {
  _filename = filename,
  _title = title,
  _published = published,
  _layout = layout,
  _content = content,
  _targetPath = targetPath,
  _isDraft = isDraft })

data Config = Config
  { configLayoutDirectory :: !FilePath,
    configOutputDirectory :: !FilePath,
    configPostsDirectory  :: !FilePath,
    configDefaultLayout   :: !Text,
    configLayoutExts      :: ![String],
    configPageExts        :: ![String],
    configName            :: !Text,
    configSiteUrl         :: !Text,
    configRenderDrafts    :: !Bool
  }
  deriving (Show)

instance Default Config where
  def :: Config
  def =
    Config
      { configLayoutDirectory = "layouts",
        configOutputDirectory = "site",
        configPostsDirectory = "posts",
        configDefaultLayout = "base",
        configLayoutExts = [".jinja2"],
        configPageExts = [".md", ".html"],
        configName = "My Website",
        configSiteUrl = "https://example.com/",
        configRenderDrafts = False
      }

mapM mkOptional [''Config]

data SpinselState = SpinselState
  { posts           :: [Page 'Raw]
  }

type SpinselError = Text

type Spinsel = StateT SpinselState (ExceptT SpinselError (ReaderT Config IO))

type StatelessSpinsel = ExceptT SpinselError (ReaderT Config IO)

runStatelessSpinsel :: StatelessSpinsel a -> Config -> IO (Either Text a)
runStatelessSpinsel = runReaderT . runExceptT

runSpinsel :: Spinsel a -> SpinselState -> Config -> IO (Either Text a)
runSpinsel = (runStatelessSpinsel .) . evalStateT

withSpinselState :: SpinselState -> Spinsel a -> StatelessSpinsel a
withSpinselState = flip evalStateT

wrap :: Monad n => ReaderT r m a -> ReaderT r n (m a)
wrap = mapReaderT pure
