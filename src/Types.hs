module Types where

import ClassyPrelude hiding (FilePath)
import Control.Lens (set, view)
import Control.Lens.TH (makeLenses)
import Data.Aeson ((.:), FromJSON, parseJSON, withArray, withObject)
import Turtle (FilePath, decodeString, realpath)

data Config = Config
  { _configProjects :: [ConfigProject]
  } deriving (Eq, Show)

data ConfigProject = ConfigProject
  { _configProjectName :: Text
  , _configProjectHome :: FilePath
  , _configProjectDirs :: [FilePath]
  } deriving (Eq, Show)

makeLenses ''Config
makeLenses ''ConfigProject

defaultConfig :: Config
defaultConfig = Config []

-- |Resolve links to `$HOME` etc.
refineConfig :: MonadIO m => Config -> m Config
refineConfig = map Config . traverse refineConfigProject . view configProjects
  where
    refineConfigProject project = do
      refinedHome <- realpath $ view configProjectHome project
      pure $ set configProjectHome refinedHome project

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \ obj -> ConfigProject
    <$> obj .: "name"
    <*> (decodeString <$> obj .: "home")
    <*> map (map decodeString) (obj .: "dirs")

instance FromJSON Config where
  parseJSON = withArray "Config" $ \ arr ->
    Config . toList <$> traverse parseJSON arr
