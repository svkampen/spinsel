{-# LANGUAGE BlockArguments, QuasiQuotes #-}

module Main (main) where

import Data.Text    qualified as T
import Data.Text.IO qualified as TIO
import Lib          (copyAssets, runSS)
import Page         (filterPosts, findPages)
import Render       (compileAllLayoutTemplates, generatePage)
import Types        (SpinselState (..), withSpinselState)

showError :: Either T.Text a -> IO ()
showError (Left err) = TIO.putStrLn err
showError (Right _)  = pure ()

main :: IO ()
main =
  showError =<< runSS do
    rawPages <- findPages
    rawPosts <- filterPosts rawPages

    layoutTemplates <- compileAllLayoutTemplates

    let state = SpinselState {posts = rawPosts, layoutTemplates = layoutTemplates}
    withSpinselState state do
      mapM_ generatePage rawPages

    copyAssets
