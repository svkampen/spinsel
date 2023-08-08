{-# LANGUAGE BlockArguments, DataKinds, TypeFamilies, UndecidableInstances #-}

module Types
  ( Config (..),
    wrap,
    Spinsel,
    StatelessSpinsel,
    SpinselState (..),
    runSpinsel,
    runStatelessSpinsel,
    withSpinselState,
    Page (..),
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
import Data.Text            (Text)
import Data.Time            (Day)
import Text.Mustache        (Template)
import Text.Mustache.Types  (TemplateCache)

data PageState = Raw | Rendered | Compiled | Templated

type family ContentType (a :: PageState)

type instance ContentType 'Raw = Text
type instance ContentType 'Rendered = Text
type instance ContentType 'Compiled = Template
type instance ContentType 'Templated = Text

type Layout = Text

type TemplateName = Text

data Page (a :: PageState) = Page
  { filename   :: FilePath,
    title      :: Text,
    published  :: Day,
    layout     :: Layout,
    content    :: ContentType a,
    targetPath :: Maybe FilePath
  }

deriving instance Show (ContentType a) => Show (Page a)

data Config = Config
  { configLayoutDirectory :: !FilePath,
    configOutputDirectory :: !FilePath,
    configPostsDirectory  :: !FilePath,
    configDefaultLayout   :: !Text,
    configTemplateExts    :: ![String],
    configName            :: !Text
  }
  deriving (Show)

data SpinselState = SpinselState
  { posts           :: [Page 'Raw],
    layoutTemplates :: TemplateCache
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
