import ClassyPrelude
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (shell)
import Turtle (ExitCode(ExitSuccess), encodeString, exit, systemStrictWithErr)
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
  forM_ subdirs $ \ subdir -> do
    putStrLn $ "# " <> pack (encodeString subdir)
    let command = shell $ "cd " <> encodeString subdir <> " && git " <> unpack optsCommand
    (code, out, err) <- systemStrictWithErr command mempty
    traverse_ (hPutStrLn stdout . unpack) $ lines out
    traverse_ (hPutStrLn stderr . unpack) $ lines err
    case code == ExitSuccess of
      False -> exit code
      True -> pure ()
