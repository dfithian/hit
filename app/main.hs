import ClassyPrelude hiding ((</>))
import Control.Lens (view)
import Data.Yaml (decodeFileThrow)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (shell)
import Turtle (ExitCode(ExitSuccess), (</>), encodeString, exit, home, systemStrictWithErr)
import qualified Options.Applicative as Opt

import Command (interpretSubdirs)
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
    putStrLn $ "# " <> pack (encodeString subdir)
    let command = shell $ "cd " <> encodeString subdir <> " && git " <> unpack (unwords innerCommand)
    (code, out, err) <- systemStrictWithErr command mempty
    traverse_ (hPutStrLn stdout . unpack) $ lines out
    traverse_ (hPutStrLn stderr . unpack) $ lines err
    case code == ExitSuccess of
      False -> exit code
      True -> pure ()
