{-# LANGUAGE BlockArguments, QuasiQuotes #-}
module Lib (runS, runSS, copyAssets) where

import Config                  (loadConfig)
import Data.String.Interpolate (i)
import Data.Text               (Text)
import Types                   (Config (..), Spinsel, SpinselState (..), StatelessSpinsel, asks,
                                evalStateT, liftIO, runStatelessSpinsel)
import Utils                   (copyDir)

runS :: Spinsel r -> SpinselState -> IO (Either Text r)
runS = (runSS .) . evalStateT

runSS :: StatelessSpinsel r -> IO (Either Text r)
runSS action = do
  Right c <- loadConfig
  runStatelessSpinsel action c

copyAssets :: StatelessSpinsel ()
copyAssets = do
  outputDir <- asks configOutputDirectory

  liftIO do
    putStrLn "Copying assets..."
    copyDir "./static" [i|./#{outputDir}/|]
    putStrLn [i|Updated #{outputDir}/|]
