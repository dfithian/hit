module Command where

import ClassyPrelude hiding (FilePath, fold)
import Turtle
  ( FoldShell(FoldShell), FilePath, directory, dirname, encodeString, foldShell, isDirectory, ls
  , pwd, stat
  )

import qualified Types as T

interpretSubdirs :: MonadIO m => T.Location -> m [FilePath]
interpretSubdirs loc = do
  cwd <- pwd
  let isGitSubdir accum next = do
        nextStatus <- stat next
        pure $ case accum of
          Nothing -> case isDirectory nextStatus && dirname next == ".git" of
            True -> Just next
            False -> Nothing
          isGit -> isGit
      gitDir accum next = do
        nextStatus <- stat next
        case isDirectory nextStatus of
          False -> pure accum
          True -> foldShell (ls next) (FoldShell isGitSubdir Nothing pure) >>= \ case
            Just _ -> pure $ next:accum
            Nothing -> pure accum
  subdirs <- foldShell (ls cwd) $ FoldShell gitDir [] pure
  case loc of
    T.LocationAll -> pure subdirs
    T.LocationSubdir x -> case x `elem` map directory subdirs of
      False -> fail $ encodeString x <> " is not a subdir of " <> encodeString cwd
      True -> pure [x]
