{-# LANGUAGE BlockArguments, DataKinds, QuasiQuotes #-}

module Lib (generatePage, copyAssets) where

import Data.String.Interpolate (i)
import Data.Text.IO            qualified as TIO
import Page                    (generateUrl)
import Render                  (renderPage)
import System.Directory        (createDirectoryIfMissing)
import System.FilePath         (takeDirectory, (</>))
import Template                (templatePage)
import Types                   (Config (..), Page (..), PageState (..), Spinsel, StatelessSpinsel,
                                asks, lift, liftIO)
import Utils                   (copyDir)

copyAssets :: StatelessSpinsel ()
copyAssets = do
  outputDir <- asks configOutputDirectory

  liftIO do
    putStrLn "Copying assets..."
    copyDir "./static" [i|./#{outputDir}/|]
    putStrLn [i|Updated #{outputDir}/|]

generatePage :: Page 'Raw -> Spinsel (Page 'Templated)
generatePage page = do
  liftIO $ putStrLn [i|Generating page #{filename page}|]
  outputDir <- asks configOutputDirectory
  liftIO $ putStrLn "Rendering..."
  renderedPage <- lift $ renderPage page
  liftIO $ putStrLn "Templating..."
  page' <- templatePage renderedPage

  let outputPath = outputDir </> ("." ++ generateUrl page)
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory outputPath)
    TIO.writeFile outputPath (content page')
  pure page'
