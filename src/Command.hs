module Command where

import Prelude

import Data.Text (Text, unpack)
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import System.FilePath.Posix ((</>), takeDirectory)
import qualified Data.Text.ANSI as Text.ANSI

import qualified Types as T

data Recurse
  = RecurseNone
  | RecurseOnce
  deriving (Eq, Ord, Show, Enum, Bounded)

-- |Fold over directories and accumulate the ones that contain a `.git` folder or whose children contain a `.git` folder
-- up to the specified recursion depth.
foldGitDirs :: (Eq a, Bounded a, Enum a) => a -> [FilePath] -> FilePath -> IO [FilePath]
foldGitDirs recurse accum next = do
  isDirectory <- doesDirectoryExist next
  case (isDirectory, next == ".git") of
    (False, _) -> pure accum
    (_, True) -> pure $ (takeDirectory next):accum
    (True, False) -> case recurse == minBound of
      True -> pure accum
      False -> do
        dirs <- listDirectory next
        mconcat <$> traverse (foldGitDirs (pred recurse) accum) dirs

-- |Interpret subdirs. If a project is specified by `name` in `main.hs`, then we interpret the subdirs to be the ones
-- from that project's config. If not, check the current working directory and return every child that has a `.git`
-- subdirectory. If we're currently in a git repository, also return that one.
interpretSubdirs :: Maybe T.ConfigProject -> IO [FilePath]
interpretSubdirs = \case
  Just project -> do
    let projectHome = T.configProjectHome project
        projectDirs = T.configProjectDirs project
    pure $ (projectHome </>) <$> projectDirs
  Nothing -> do
    cwd <- getCurrentDirectory
    dirs <- listDirectory cwd
    mconcat <$> traverse (foldGitDirs RecurseOnce []) dirs

-- |Print a command from this program, always in blue and always unset after.
putStrLnComment :: Text -> IO ()
putStrLnComment x = putStrLn $ unpack $ Text.ANSI.cyan $ "# " <> x
