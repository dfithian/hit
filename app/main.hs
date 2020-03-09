import ClassyPrelude hiding ((</>))
import Control.Lens (view)
import Data.Yaml (decodeFileThrow)
import System.Directory (doesFileExist)
import System.Process (shell)
import Turtle (ExitCode(ExitSuccess), (</>), encodeString, exit, home, system)
import qualified Options.Applicative as Opt

import Command (interpretSubdirs, putStrLnComment)
import qualified Types as T

data Opts = Opts
  { optsCommand :: [Text]
  }

parseArgs :: IO Opts
parseArgs = Opt.execParser $ Opt.info parser mods
  where
    commandArgument = some (pack <$> Opt.strArgument (Opt.metavar "COMMAND"))
    parser = Opts
      <$> commandArgument
    mods = Opt.header "Hit - Git++"

main :: IO ()
main = do
  Opts {..} <- parseArgs
  configFile <- (</> ".hitconfig") <$> home
  let configFileStr = encodeString configFile
  config <- doesFileExist configFileStr >>= \ case
    True -> decodeFileThrow configFileStr >>= T.refineConfig
    False -> pure T.defaultConfig
  (projectMay, innerCommand) <- case optsCommand of
    x:xs -> case find (\ proj -> view T.configProjectName proj == x) $ view T.configProjects config of
      Just project -> pure (Just project, xs)
      Nothing -> pure (Nothing, optsCommand)
    [] -> fail "No command specified"
  subdirs <- interpretSubdirs projectMay
  forM_ subdirs $ \ subdir -> do
    putStrLnComment $ "# " <> pack (encodeString subdir)
    let command = shell $ "cd " <> encodeString subdir <> " && git " <> unpack (unwords innerCommand)
    code <- system command mempty
    case code == ExitSuccess of
      False -> exit code
      True -> pure ()
