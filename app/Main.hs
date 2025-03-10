{-# LANGUAGE BlockArguments, QuasiQuotes #-}

module Main (main) where

import Config       (getConfig)
import Data.List    ((\\))
import Data.Text    qualified as T
import Data.Text.IO qualified as TIO
import Feed         (generateFeed)
import Lib          (copyAssets, generatePage)
import Page         (filterPosts, findPages, isDraft)
import Types        (SpinselState (..), withSpinselState)
import Utils        (runSS)

showError :: Either T.Text a -> IO ()
showError (Left err) = TIO.putStrLn err
showError (Right _)  = pure ()

main :: IO ()
main = do
  Right config <- getConfig
  showError =<< runSS config do
    rawPages <- findPages
    rawPosts <- filterPosts rawPages

    let state = SpinselState {posts = rawPosts}
    withSpinselState state do
      completePosts <- mapM generatePage rawPosts
      mapM_ generatePage (rawPages \\ rawPosts)

      let feedPosts = filter (not . isDraft) completePosts
      generateFeed feedPosts-- temporarily disabled due to content issues

    copyAssets
