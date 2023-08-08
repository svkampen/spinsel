{-# LANGUAGE LambdaCase, QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utils where

import Control.Monad
import Control.Monad.Except    (liftEither)
import Data.HashMap.Lazy       qualified as HM
import Data.List               (isSuffixOf)
import Data.Maybe              (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text               qualified as T
import Prelude                 hiding (pred)
import System.Directory        (doesDirectoryExist, listDirectory)
import System.Exit             (ExitCode (..))
import System.FilePath         ((</>))
import System.Info qualified
import System.Process          (system)
import Text.Mustache.Types     qualified as M
import Types                   (StatelessSpinsel)

-- | List directory, giving the path including the directory.
listDirectory' :: FilePath -> IO [FilePath]
listDirectory' dir = map (dir </>) <$> listDirectory dir

-- | Recursively traverse a directory, finding files matching exts, excluding the dirs given in excludeDirs.
getMatchingFiles :: [String] -> [FilePath] -> FilePath -> IO [FilePath]
getMatchingFiles exts excludeDirs = go
  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      entries <- map (dir </>) <$> listDirectory dir
      (dirs, files) <- partitionM doesDirectoryExist entries
      let matchingFiles = filter (matchesExt exts) files
          dirs' = filter (not . (`elem` excludeDirs)) dirs
      matchingFilesLower <- concat <$> mapM go dirs'
      pure $ matchingFiles ++ matchingFilesLower

orElse :: Maybe c -> c -> c
orElse = flip fromMaybe

-- Like filterM, but for partition. This is a bad implementation, but whatever.
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred lst = (,) <$> filterM pred lst <*> filterM (fmap not . pred) lst

matchesExt :: [String] -> FilePath -> Bool
matchesExt exts fp = any (`isSuffixOf` fp) exts

tshow :: Show a => a -> T.Text
tshow = T.pack . show

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just r) = Right r
maybeToEither l Nothing  = Left l

maybeToSpinsel :: T.Text -> Maybe b -> StatelessSpinsel b
maybeToSpinsel = (liftEither .) . maybeToEither

-- | Optimized copy on macOS using clonefile
copyDirMacOS :: FilePath -> FilePath -> IO ExitCode
copyDirMacOS src dest = system [i|cp -Rc #{src} #{dest}|]

-- | Optimized copy on Linux using reflinks
copyDirLinux :: FilePath -> FilePath -> IO ExitCode
copyDirLinux src dest = system [i|cp -r --reflink=always #{src} #{dest}|]

-- | Generic copy
copyDirGeneric :: FilePath -> FilePath -> IO ExitCode
copyDirGeneric src dest = system [i|cp -r #{src} #{dest}|]

copyDir' :: FilePath -> FilePath -> IO ExitCode
copyDir' = case System.Info.os of
  "darwin" -> copyDirMacOS
  "linux"  -> copyDirLinux
  _        -> copyDirGeneric

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dest =
  copyDir' src dest >>= \case
    ExitFailure code -> error [i|copyDir #{src} #{dest} failed with exit code #{code}|]
    ExitSuccess -> pure ()

unionObjects :: M.Value -> M.Value -> M.Value
unionObjects (M.Object o) (M.Object o') = M.Object $ HM.union o o'
unionObjects _ _                        = error "unionObjects used with non-object values"
