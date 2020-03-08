module Command where

import ClassyPrelude hiding (FilePath, fold)
import Turtle (FoldShell(FoldShell), FilePath, basename, foldShell, isDirectory, ls, pwd, stat)

interpretSubdirs :: MonadIO m => m [FilePath]
interpretSubdirs = do
  cwd <- pwd
  let isGitSubdir accum next = do
        nextStatus <- stat next
        case accum of
          Nothing -> case isDirectory nextStatus && basename next == ".git" of
            True -> pure $ Just next
            False -> pure Nothing
          isGit -> pure isGit
      gitDir accum next = do
        nextStatus <- stat next
        case (isDirectory nextStatus, basename next == ".git") of
          (False, False) -> pure accum
          (_, True) -> pure $ next:accum
          (True, _) -> foldShell (ls next) (FoldShell isGitSubdir Nothing pure) >>= \ case
            Just _ -> pure $ next:accum
            Nothing -> pure accum
  foldShell (ls cwd) $ FoldShell gitDir [] pure
