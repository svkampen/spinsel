{-# LANGUAGE BlockArguments, DataKinds, ImportQualifiedPost, OverloadedRecordDot, OverloadedStrings,
             QuasiQuotes, TypeFamilies, UndecidableInstances #-}

module Page (readPage, readPageByName, findTemplate, filterPosts, findPages, findTemplatesInDirs, Page (..), PageState (..)) where

import Control.Monad           (filterM)
import Control.Monad.Except    (throwError)
import Data.List               (isPrefixOf)
import Data.List.Split         (splitOn)
import Data.String.Interpolate (i)
import Data.Text               (Text)
import Data.Text               qualified as T
import Data.Text.IO            qualified as TIO
import Data.Time               (Day, UTCTime (utctDay), defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX   (posixSecondsToUTCTime)
import Foreign.C.Types         (CTime)
import Parser qualified
import System.FilePath         (normalise, takeBaseName, (</>))
import System.Posix.Files      (FileStatus, fileExist, getFileStatus, modificationTime)
import Types                   (Config (..), MonadIO (liftIO), Page (..), PageState (..),
                                StatelessSpinsel, TemplateName, asks, guard)
import Utils                   (getMatchingFiles, listDirectory', matchesExt, orElse)

parseFilename :: FilePath -> Maybe (Day, Text)
parseFilename path = do
  let name = takeBaseName path
      parts = splitOn "-" name

  guard (length parts > 3)

  let (dateParts, titleParts) = splitAt 3 parts
      title = T.toTitle . T.unwords $ T.pack <$> titleParts

  day <- parseTimeM True defaultTimeLocale "%Y %m %d" (unwords dateParts) :: Maybe Day

  pure (day, title)

titleFromFilename :: FilePath -> Text
titleFromFilename = T.toTitle . T.replace "-" " " . T.pack . takeBaseName

ctimeToUTCTime :: CTime -> UTCTime
ctimeToUTCTime = posixSecondsToUTCTime . realToFrac

constructPage :: FilePath -> FileStatus -> Text -> StatelessSpinsel (Page 'Raw)
constructPage fileName fileStatus contents = do
  defLayout <- asks configDefaultLayout

  Parser.Page fm body <-
    maybe
      (throwError [i|"Unable to parse page #{fileName}"|])
      return
      (Parser.parsePage fileName contents)

  let layoutName = lookup "layout" fm `orElse` defLayout
  layoutFile <- head <$> findTemplate layoutName
  layoutText <- liftIO $ TIO.readFile layoutFile

  let (publishDay, title) = parseFilename fileName `orElse` (utctDay . ctimeToUTCTime . modificationTime $ fileStatus, titleFromFilename fileName)

  pure
    Page
      { filename = fileName,
        title = lookup "title" fm `orElse` title,
        published = publishDay,
        layout = layoutText,
        content = body,
        targetPath = normalise . T.unpack <$> lookup "target" fm,
        isDraft = (== Just "true") . lookup "draft" $ fm
      }

generatePossibleFiles :: String -> [FilePath] -> [String] -> [FilePath]
generatePossibleFiles name dirs exts = do
  dir <- dirs
  ext <- exts
  pure (dir </> name ++ ext)

findTemplatesInDirs :: [FilePath] -> StatelessSpinsel [FilePath]
findTemplatesInDirs dirs = do
  templateExts <- asks configTemplateExts
  let hasTemplateExt = matchesExt templateExts
  files <- concat <$> liftIO (mapM listDirectory' dirs)
  pure $ filter hasTemplateExt files

-- | Find template in the configured search directories
findTemplate :: TemplateName -> StatelessSpinsel [FilePath]
findTemplate name = do
  layoutDir <- asks configLayoutDirectory
  templateExts <- asks configTemplateExts
  let dirs = [layoutDir, "./"]
      exts = templateExts
      fnames = generatePossibleFiles (T.unpack name) dirs exts
  liftIO $ filterM fileExist fnames

readPageByName :: TemplateName -> StatelessSpinsel (Page 'Raw)
readPageByName name = do
  fname <- head <$> findTemplate name
  fileStatus <- liftIO $ getFileStatus fname
  contents <- liftIO $ TIO.readFile fname
  constructPage fname fileStatus contents

readPage :: FilePath -> StatelessSpinsel (Page 'Raw)
readPage path = do
  fileStatus <- liftIO $ getFileStatus path
  contents <- liftIO $ TIO.readFile path
  constructPage path fileStatus contents

-- | Find all pages recursively from the current directory
findPages :: StatelessSpinsel [Page 'Raw]
findPages = do
  exts <- asks configTemplateExts
  excludeDirs <- map ("." </>) <$> sequence [asks configLayoutDirectory, asks configOutputDirectory]
  files <- liftIO $ getMatchingFiles exts excludeDirs "."
  pages <- mapM readPage files

  -- Filter out drafts, unless we are in 'render drafts' mode
  renderDrafts <- asks configRenderDrafts
  pure $ filter ((||) <$> (not . isDraft) <*> const renderDrafts) pages


-- | Filter a list of pages, returning only posts.
filterPosts :: [Page 'Raw] -> StatelessSpinsel [Page 'Raw]
filterPosts pages = do
  postDir <- asks configPostsDirectory
  pure $ filter (([i|./#{postDir}/|] `isPrefixOf`) . filename) pages
