import Prelude

import Control.Monad (forM_)
import Data.Text (pack)
import Data.Version (showVersion)
import Data.Yaml (decodeFileThrow)
import System.Directory (XdgDirectory(XdgConfig), doesFileExist, getXdgDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import System.FilePath.Posix ((</>))
import System.Process (callProcess)
import qualified Data.Map.Strict as Map

import Command (interpretSubdirs, putStrLnComment)
import Paths_hit (version)
import qualified Types as T

data Opts = Opts
  { optsCommand :: [String]
  }

parseArgs :: IO Opts
parseArgs = do
  args <- getArgs
  case args of
    [] -> printHelp
    "--help":_ -> printHelp
    "--version":_ -> printVersion
    x:xs ->
      let getNewArg lastArg next = case lastArg of
            "-m" -> "\"" <> next <> "\""
            _ -> next
          newArgs = x:(map (uncurry getNewArg) $ args `zip` xs)
      in pure $ Opts newArgs
  where
    printHelp = do
      putStrLn "Hit - project management for git (https://github.com/dfithian/hit)"
      putStrLn "Usage: hit [PROJECT] COMMAND"
      exitWith ExitSuccess
    printVersion = do
      putStrLn $ "hit " <> showVersion version
      exitWith ExitSuccess

main :: IO ()
main = do
  Opts {..} <- parseArgs
  putStrLnComment $ "Got command \"" <> pack (unwords optsCommand) <> "\""
  configFile <- (</> "config") <$> getXdgDirectory XdgConfig "hit"
  T.Config {..} <- doesFileExist configFile >>= \case
    True -> decodeFileThrow configFile
    False -> pure T.defaultConfig
  (projectMay, innerCommand) <- case optsCommand of
    x:xs -> case Map.lookup (pack x) configProjects of
      Just project -> pure (Just project, xs)
      Nothing -> pure (Nothing, optsCommand)
    [] -> fail "No command specified"
  subdirs <- interpretSubdirs projectMay
  forM_ subdirs $ \subdir -> do
    putStrLnComment $ pack subdir
    callProcess "git" (["-C", subdir] <> innerCommand)
