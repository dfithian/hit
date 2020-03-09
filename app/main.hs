import ClassyPrelude hiding ((</>))
import Control.Lens (view)
import Data.Version (showVersion)
import Data.Yaml (decodeFileThrow)
import System.Directory (doesFileExist)
import Turtle (ExitCode(ExitSuccess), (</>), encodeString, exit, home, shell)

import Command (interpretSubdirs, putStrLnComment)
import Paths_hit (version)
import qualified Types as T

data Opts = Opts
  { optsCommand :: [Text]
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
      putStrLn "Usage: hit COMMAND"
      exit ExitSuccess
    printVersion = do
      putStrLn $ "hit " <> pack (showVersion version)
      exit ExitSuccess

main :: IO ()
main = do
  Opts {..} <- parseArgs
  putStrLnComment $ "Got command \"" <> unwords optsCommand <> "\""
  configFile <- (</> ".hitconfig") <$> home
  let configFileStr = encodeString configFile
  config <- doesFileExist configFileStr >>= \ case
    True -> decodeFileThrow configFileStr
    False -> pure T.defaultConfig
  (projectMay, innerCommand) <- case optsCommand of
    x:xs -> case find (\ proj -> view T.configProjectName proj == x) $ view T.configProjects config of
      Just project -> pure (Just project, xs)
      Nothing -> pure (Nothing, optsCommand)
    [] -> fail "No command specified"
  subdirs <- interpretSubdirs projectMay
  forM_ subdirs $ \ subdir -> do
    putStrLnComment $ pack (encodeString subdir)
    let command = "cd " <> pack (encodeString subdir) <> " && git " <> unwords innerCommand
    code <- shell command mempty
    case code == ExitSuccess of
      False -> exit code
      True -> pure ()
