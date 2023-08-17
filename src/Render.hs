{-# LANGUAGE BlockArguments, DataKinds, DeriveGeneric, ImportQualifiedPost, LambdaCase,
             OverloadedRecordDot, OverloadedStrings, QuasiQuotes, TypeFamilies,
             UndecidableInstances #-}

module Render where

import CMark                   (commonmarkToHtml)
import CMark qualified
import Control.Arrow           (left)
import Control.Monad.Except    (MonadIO (liftIO), MonadTrans (lift), liftEither)
import Data.HashMap.Lazy       qualified as HM
import Data.List               (intercalate)
import Data.List.Split         (splitOn)
import Data.String.Interpolate (i)
import Data.Text               (Text)
import Data.Text               qualified as T
import Data.Text.IO            qualified as TIO
import Data.Text.Lazy          qualified as TL
import Data.Time.Format        (defaultTimeLocale, formatTime)
import Lucid                   qualified as L
import Page                    (Page (..), PageState (Compiled, Raw, Rendered, Templated),
                                findTemplatesInDirs)
import System.Directory        (createDirectoryIfMissing)
import System.FilePath         (normalise, replaceExtension, takeBaseName, takeDirectory, (</>))
import Text.Megaparsec         qualified as MP
import Text.MMark              qualified as MMark
import Text.Mustache           (Template (..), ToMustache (..), object, substitute, (~>))
import Text.Mustache.Compile   (TemplateCache, compileTemplate)
import Text.Mustache.Types     (Value (..))
import Types                   (Config (..), Spinsel, SpinselState (..), StatelessSpinsel, asks,
                                gets)
import Utils                   (orElse, tshow, unionObjects)

renderPage :: Page 'Raw -> Either Text (Page 'Rendered)
renderPage pst = case MMark.parse pst.filename pst.content of
  Left errs -> Left . T.pack $ MP.errorBundlePretty errs
  Right r ->
    let renderedContent = TL.toStrict . L.renderText . MMark.render $ r
     in Right pst {content = renderedContent}

renderPage' :: Page 'Raw -> Either Text (Page 'Rendered)
renderPage' pst = Right pst {content = commonmarkToHtml [CMark.optUnsafe] pst.content}

-- | Get a TemplateCache of all templates in the configured layout directory.
compileAllLayoutTemplates :: StatelessSpinsel TemplateCache
compileAllLayoutTemplates = do
  layoutDir <- asks configLayoutDirectory

  templatePaths <- findTemplatesInDirs [layoutDir]
  templateContents <- liftIO $ mapM TIO.readFile templatePaths

  let templateNames = takeBaseName <$> templatePaths

  templates <- sequence $ compileTemplate' <$> templatePaths <*> templateContents
  pure $ HM.fromList (zip templateNames templates)

compileTemplate' :: String -> Text -> StatelessSpinsel Template
compileTemplate' = (liftEither .) . (left tshow .) . compileTemplate

-- | Compile a page, yielding a Page 'Compiled with a fully populated Template.
compilePage :: Page 'Rendered -> Spinsel (Page 'Compiled)
compilePage page = do
  compiledPage <- lift $ compileTemplate' "content" (content page)
  compiledLayout <- lift $ compileTemplate' "layout" (layout page)

  templateCache <- gets layoutTemplates

  -- insert compiled page as "content" partial
  let partials' = HM.insert "content" compiledPage templateCache
      compiledLayout' = compiledLayout {partials = partials'}

  let templatedPage = page {content = compiledLayout'}

  pure templatedPage

templatePage :: ToMustache k => k -> Page 'Compiled -> Page 'Templated
templatePage value page@Page {content = content'} = page {content = substitute content' value}

-- | Generate a URL for a given page.
generateUrl :: Page a -> String
generateUrl page =
  let path = targetPath page `orElse` filename page
   in "/" </> replaceExtension (normalise path) "html"

-- | Generate a formatted publish date for a given page.
publishDate :: Page a -> String
publishDate page = formatTime defaultTimeLocale "%A, %B %e, %Y" (published page)

generatePostsData :: [Page 'Raw] -> Value
generatePostsData posts = object ["posts" ~> map genPostData posts]
  where
    genPostData :: Page 'Raw -> Value
    genPostData page = object ["title" ~> title page, "url" ~> generateUrl page, "published" ~> publishDate page]

genIntermediatePaths :: [String] -> [String]
genIntermediatePaths path_elems = genPathOfLengthAtMost <$> [1 .. length path_elems]
  where
    genPathOfLengthAtMost :: Int -> String
    genPathOfLengthAtMost n = intercalate "/" (take n path_elems)

-- | Generate template data for a page, given the page and global state
generateTemplateData :: Page 'Raw -> Spinsel Value
generateTemplateData page@Page {title} = do
  name <- asks configName
  posts <- gets posts

  let pageTitle = [i|#{title} - #{name}|] :: String
      url = generateUrl page
      url_parts = splitOn "/" url
      url_parts' = "/" : tail url_parts
      url_intermediates = genIntermediatePaths url_parts
      url_intermediates' = "/" : tail url_intermediates
      url_components = (\(part, path) -> object ["elem" ~> part, "intermediate_path" ~> path]) <$> zip url_parts' url_intermediates'

  pure $
    unionObjects
      ( object
          [ "title" ~> title,
            "pageTitle" ~> pageTitle,
            "url" ~> url,
            "url_components" ~> url_components,
            "published" ~> publishDate page
          ]
      )
      (generatePostsData posts)

generatePage :: Page 'Raw -> Spinsel (Page 'Templated)
generatePage page = do
  liftIO $ putStrLn [i|Generating page #{filename page}|]
  outputDir <- asks configOutputDirectory
  renderedPage <- liftEither (renderPage' page)
  templateData <- generateTemplateData page
  page' <- templatePage templateData <$> compilePage renderedPage

  let outputPath = outputDir </> ("." ++ generateUrl page)
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory outputPath)
    TIO.writeFile outputPath (content page')
  pure page'
