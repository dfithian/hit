module Command where

import ClassyPrelude hiding ((</>), FilePath, fold)
import Control.Lens (view)
import System.Console.ANSI (Color(Blue), ColorIntensity(Vivid), ConsoleLayer(Foreground), SGR(Reset, SetColor), setSGR)
import System.IO (stdout)
import Turtle (FoldShell(FoldShell), (</>), FilePath, basename, foldShell, isDirectory, ls, parent, pwd, stat)

import qualified Types as T

data Recurse
  = RecurseNone
  | RecurseOnce
  deriving (Eq, Ord, Show, Enum, Bounded)

-- |Fold over directories and accumulate the ones that contain a `.git` folder or whose children contain a `.git` folder
-- up to the specified recursion depth.
foldGitDirs :: (Eq a, Bounded a, Enum a, MonadIO m) => a -> [FilePath] -> FilePath -> m [FilePath]
foldGitDirs recurse accum next = do
  nextStatus <- stat next
  case (isDirectory nextStatus, basename next == ".git") of
    (False, _) -> pure accum
    (_, True) -> pure $ (parent next):accum
    (True, False) -> case recurse == minBound of
      True -> pure accum
      False -> foldShell (ls next) (FoldShell (foldGitDirs (pred recurse)) accum pure)

-- |Interpret subdirs. If a project is specified by `name` in `main.hs`, then we interpret the subdirs to be the ones
-- from that project's config. If not, check the current working directory and return every child that has a `.git`
-- subdirectory. If we're currently in a git repository, also return that one.
interpretSubdirs :: MonadIO m => Maybe T.ConfigProject -> m [FilePath]
interpretSubdirs = \ case
  Just project -> do
    let projectHome = view T.configProjectHome project
        projectDirs = view T.configProjectDirs project
    pure $ (projectHome </>) <$> projectDirs
  Nothing -> do
    cwd <- pwd
    foldShell (ls cwd) (FoldShell (foldGitDirs RecurseOnce) [] pure)

-- |Print a command from this program, always in blue and always unset after.
putStrLnComment :: MonadIO m => Text -> m ()
putStrLnComment x = liftIO $ do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn $ "# " <> x
  setSGR [Reset]
  hFlush stdout -- flush so that the reset hits the terminal
