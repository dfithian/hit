import ClassyPrelude
import System.IO (hPutStrLn, stderr, stdout)
import Turtle (ExitCode(ExitSuccess), decodeString, encodeString, exit, procStrictWithErr)
import qualified Options.Applicative as Opt

import Command (interpretSubdirs)
import qualified Types as T

data Opts = Opts
  { optsLocation :: T.Location
  , optsCommand :: Text
  }

parseArgs :: IO Opts
parseArgs = Opt.execParser $ Opt.info parser mods
  where
    readLocation = Opt.maybeReader $ \ case
      "all" -> pure T.LocationAll
      x -> pure . T.LocationSubdir . decodeString $ x
    locationOption = Opt.option readLocation (Opt.long "location" <> Opt.short 'l' <> Opt.help "subdir location (defaults to all)" <> Opt.value T.LocationAll)
    commandArgument = pack <$> Opt.strArgument (Opt.metavar "COMMAND")
    parser = Opts
      <$> locationOption
      <*> commandArgument
    mods = Opt.header "Hit - Git++"

main :: IO ()
main = do
  Opts {..} <- parseArgs
  subdirs <- interpretSubdirs optsLocation
  let command = words optsCommand
  forM_ subdirs $ \ subdir -> do
    (code, out, err) <- procStrictWithErr "git" (["-C", pack $ encodeString subdir] <> command) mempty
    traverse_ (hPutStrLn stdout . unpack) $ lines out
    traverse_ (hPutStrLn stderr . unpack) $ lines err
    case code == ExitSuccess of
      False -> exit code
      True -> pure ()
