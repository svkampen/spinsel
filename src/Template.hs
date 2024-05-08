{-# LANGUAGE DataKinds, OverloadedStrings, QuasiQuotes #-}
module Template where

import Control.Monad.Except    (MonadError (throwError))
import Data.List               (intercalate, stripPrefix)
import Data.List.Split         (splitOn)
import Data.Maybe              (listToMaybe)
import Data.String.Interpolate (i)
import Data.Text               qualified as T
import Page                    (generateUrl, publishDate)
import System.FilePath         (takeBaseName)
import TemplateFunctions       (gfnEmoji)
import Text.Ginger             (GVal, Pair, dict, makeContextHtml, runGinger, (~>))
import Text.Ginger             qualified as Ginger
import Text.Ginger.Html        (htmlSource)
import Types                   (Config (..), Page (..), PageState (..), Spinsel, SpinselState (..),
                                StatelessSpinsel, asks, gets, lift, liftIO)
import Utils                   (getMatchingFiles, orElse, tshow)


generatePostsData :: forall m. [Page 'Raw] -> Spinsel [Pair m]
generatePostsData posts = do
  postsData <- mapM genPostData posts
  pure ["posts" ~> postsData, "lastPost" ~> take 1 postsData]
  where
    genPostData :: Page 'Raw -> Spinsel (GVal m)
    genPostData page = do
      pure $ dict ["title" ~> title page, "url" ~> generateUrl page, "published" ~> publishDate page]

genIntermediatePaths :: [String] -> [String]
genIntermediatePaths path_elems = genPathOfLengthAtMost <$> [1 .. length path_elems]
  where
    genPathOfLengthAtMost :: Int -> String
    genPathOfLengthAtMost n = intercalate "/" (take n path_elems)

-- | Generate template data for a page, given the page and global state
generateTemplateData :: forall m a p h. Monad m => Page a -> Spinsel [Pair (Ginger.Run p m h)]
generateTemplateData page = do
  name <- asks configName
  posts <- gets posts

  let pageTitle = [i|#{title page} – #{name}|] :: String
      url = generateUrl page
      url_parts = splitOn "/" url
      url_parts' = "/" : tail url_parts
      url_intermediates = genIntermediatePaths url_parts
      url_intermediates' = "/" : tail url_intermediates
      url_components = (\(part, path) -> dict @(Ginger.Run p m h) ["elem" ~> part, "intermediate_path" ~> path]) <$> zip url_parts' url_intermediates'

  postsData <- generatePostsData posts

  let ps = [ "title" ~> title page,
             "pageTitle" ~> pageTitle,
             "url" ~> url,
             "url_components" ~> url_components,
             "published" ~> publishDate page,
             "emoji" ~> Ginger.fromFunction (gfnEmoji @p @m @h)
           ] ++ postsData

  pure ps

-- | Find layout in the configured search directories
findLayout :: Ginger.SourceName -> StatelessSpinsel [FilePath]
findLayout name = do
  layoutDir <- asks configLayoutDirectory
  layoutExts <- asks configLayoutExts
  files <- liftIO $ getMatchingFiles layoutExts [] layoutDir
  pure $ filter ((name ==) . takeBaseName) files

-- | Resolve layouts, returning their contents
layoutResolver :: Ginger.SourceName -> Spinsel (Maybe Ginger.Source)
layoutResolver name = do
  let name' = stripPrefix "./" name `orElse` name
  liftIO $ putStrLn [i|Resolving layout: #{name}|]
  matchingLayouts <- lift $ findLayout name'
  liftIO (mapM readFile (listToMaybe matchingLayouts))

-- | Resolve layouts, except for special "content" include
layoutResolver' :: T.Text -> Ginger.SourceName -> Spinsel (Maybe Ginger.Source)
layoutResolver' content name = case name of
    "./content" -> pure . Just . T.unpack $ content
    _           -> layoutResolver name

-- | Compile a page, yielding a Page 'Compiled which contains a Ginger template
compilePage :: Page 'Rendered -> Spinsel (Page 'Compiled)
compilePage page = do
  res <- Ginger.parseGingerFile (layoutResolver' (content page)) (T.unpack (layout page))
  tpl <- either (throwError . tshow) pure res
  pure page{content = tpl}

-- | Template the page, substituting variables etc
templatePage :: Page 'Rendered -> Spinsel (Page 'Templated)
templatePage pg = do
  ctx <- generateTemplateData pg
  Page{content} <- compilePage pg
  let res = htmlSource $ runGinger (makeContextHtml (scopeLookup ctx)) content
  pure pg{content = res}
  where
    scopeLookup :: [Pair m] -> T.Text -> GVal m
    scopeLookup ctx n = Ginger.toGVal (lookup n ctx)
