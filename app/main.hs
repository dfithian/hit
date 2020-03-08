import ClassyPrelude
import System.IO (hPutStrLn, stderr, stdout)
import Turtle (ExitCode(ExitSuccess), encodeString, exit, procStrictWithErr)
import qualified Options.Applicative as Opt

import Command (interpretSubdirs)

data Opts = Opts
  { optsCommand :: Text
  }

parseArgs :: IO Opts
parseArgs = Opt.execParser $ Opt.info parser mods
  where
    commandArgument = pack <$> Opt.strArgument (Opt.metavar "COMMAND")
    parser = Opts
      <$> commandArgument
    mods = Opt.header "Hit - Git++"

main :: IO ()
main = do
  Opts {..} <- parseArgs
  subdirs <- interpretSubdirs
  let command = words optsCommand
  forM_ subdirs $ \ subdir -> do
    putStrLn $ "# " <> pack (encodeString subdir)
    (code, out, err) <- procStrictWithErr "git" (["-C", pack $ encodeString subdir] <> command) mempty
    traverse_ (hPutStrLn stdout . unpack) $ lines out
    traverse_ (hPutStrLn stderr . unpack) $ lines err
    case code == ExitSuccess of
      False -> exit code
      True -> pure ()
