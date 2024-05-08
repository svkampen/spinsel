{-# LANGUAGE AllowAmbiguousTypes, BlockArguments, DataKinds, ImportQualifiedPost,
             OverloadedRecordDot, OverloadedStrings, PatternSynonyms, QuasiQuotes, TypeFamilies,
             UndecidableInstances #-}

module Page (generateUrl, publishDate, readPage, filterPosts, findPages, Page (..), PageState (..)) where

import Control.Monad           (guard)
import Control.Monad.Except    (throwError)
import Data.List               (isPrefixOf, sortOn)
import Data.List.Split         (splitOn)
import Data.Ord                (Down (Down))
import Data.String.Interpolate (i)
import Data.Text               (Text)
import Data.Text               qualified as T
import Data.Text.IO            qualified as TIO
import Data.Time               (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import Foreign.C.Types         (CTime)
import Parser qualified
import System.FilePath         (normalise, replaceExtension, takeBaseName, (</>))
import System.Posix.Files      (FileStatus, getFileStatus, modificationTime)
import Types                   (Config (..), MonadIO (liftIO), Page (..), PageState (..),
                                StatelessSpinsel, asks)
import Utils                   (getMatchingFiles, orElse)

-- | Attempt to parse a filename into a date and title.
parseFilename :: FilePath -> Maybe (Day, Text)
parseFilename path = do
  let name = takeBaseName path
      parts = splitOn "-" name

  guard (length parts > 3)

  let (dateParts, titleParts) = splitAt 3 parts
      title = T.toTitle . T.unwords $ T.pack <$> titleParts

  day <- parseTimeM True defaultTimeLocale "%Y %m %d" (unwords dateParts) :: Maybe Day

  pure (day, title)

-- | Turn a filename ("the-quick-brown-fox.md") into a title ("The quick brown fox").
titleFromFilename :: FilePath -> Text
titleFromFilename = T.toTitle . T.replace "-" " " . T.pack . takeBaseName

ctimeToUTCTime :: CTime -> UTCTime
ctimeToUTCTime = posixSecondsToUTCTime . realToFrac

-- | Construct a raw page from its path, status and contents.
constructPage :: FilePath -> FileStatus -> Text -> StatelessSpinsel (Page 'Raw)
constructPage fileName fileStatus contents = do
  defLayout <- asks configDefaultLayout

  Parser.Page fm body <-
    maybe
      (throwError [i|"Unable to parse page #{fileName}"|])
      return
      (Parser.parsePage fileName contents)

  let layoutName = lookup "layout" fm `orElse` defLayout
  -- layoutFile <- head <$> findTemplate layoutName
  -- layoutText <- liftIO $ TIO.readFile layoutFile

  let (publishDay, title) = parseFilename fileName `orElse` (utctDay . ctimeToUTCTime . modificationTime $ fileStatus, titleFromFilename fileName)

  pure Page
      { filename = fileName,
        title = lookup "title" fm `orElse` title,
        published = publishDay,
        layout = layoutName,
        content = body,
        targetPath = normalise . T.unpack <$> lookup "target" fm,
        isDraft = (== Just "true") . lookup "draft" $ fm
      }

readPage :: FilePath -> StatelessSpinsel (Page 'Raw)
readPage path = do
  fileStatus <- liftIO $ getFileStatus path
  contents <- liftIO $ TIO.readFile path
  constructPage path fileStatus contents

-- | Find all pages recursively from the current directory
findPages :: StatelessSpinsel [Page 'Raw]
findPages = do
  exts <- asks configPageExts
  excludeDirs <- map ("." </>) <$> sequence [asks configLayoutDirectory, asks configOutputDirectory]
  files <- liftIO $ getMatchingFiles exts excludeDirs "."
  pages <- mapM readPage files

  -- Filter out drafts, unless we are in 'render drafts' mode
  renderDrafts <- asks configRenderDrafts
  pure $ filter ((||) <$> (not . isDraft) <*> const renderDrafts) pages

-- | Filter a list of pages, returning only posts. Sorts posts by publish date.
filterPosts :: forall a. [Page a] -> StatelessSpinsel [Page a]
filterPosts pages = do
  postDir <- asks configPostsDirectory
  let posts = filter (([i|./#{postDir}/|] `isPrefixOf`) . filename) pages
  pure (sortOn (Down . published) posts)

-- | Generate a URL for a given page.
generateUrl :: Page a -> String
generateUrl page =
  let path = targetPath page `orElse` filename page
   in "/" </> replaceExtension (normalise path) "html"

-- | Generate a formatted publish date for a given page.
publishDate :: Page a -> String
publishDate page = formatTime defaultTimeLocale "%A, %B %e, %Y" (published page)
