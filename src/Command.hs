module Command where

import ClassyPrelude hiding ((</>), FilePath, fold)
import Control.Lens (view)
import System.Console.ANSI (Color(Blue), ColorIntensity(Vivid), ConsoleLayer(Foreground), SGR(Reset, SetColor), setSGR)
import System.IO (stdout)
import Turtle (FoldShell(FoldShell), (</>), FilePath, basename, foldShell, isDirectory, ls, parent, pwd, stat)

import qualified Types as T

-- |Interpret subdirs. If a project is specified by `name` in `main.hs`, then we interpret the subdirs to be the ones
-- from that project's config. If not, check the current working directory and return every child that has a `.git`
-- subdirectory. If we're currently in a git repository, also return that one.
interpretSubdirs :: MonadIO m => Maybe T.ConfigProject -> m [FilePath]
interpretSubdirs projectMay = do
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
          (_, True) -> pure $ (parent next):accum
          (True, _) -> foldShell (ls next) (FoldShell isGitSubdir Nothing pure) >>= \ case
            Just _ -> pure $ next:accum
            Nothing -> pure accum
  case projectMay of
    Just project -> do
      let projectHome = view T.configProjectHome project
          projectDirs = view T.configProjectDirs project
      pure $ (projectHome </>) <$> projectDirs
    Nothing -> do
      cwd <- pwd
      foldShell (ls cwd) $ FoldShell gitDir [] pure

-- |Print a command from this program, always in blue and always unset after.
putStrLnComment :: MonadIO m => Text -> m ()
putStrLnComment x = liftIO $ do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn $ "# " <> x
  setSGR [Reset]
  hFlush stdout -- flush so that the reset hits the terminal
