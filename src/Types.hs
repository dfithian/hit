module Types where

import ClassyPrelude hiding (FilePath)
import Control.Lens.TH (makeLenses)
import Data.Aeson ((.:), FromJSON, parseJSON, withArray, withObject)
import Turtle (FilePath, decodeString)

data Config = Config
  { _configProjects :: [ConfigProject]
  -- ^ The list of projects
  } deriving (Eq, Show)

data ConfigProject = ConfigProject
  { _configProjectName :: Text
  -- ^ The name of this project, which we invoke using `hit <name>`.
  , _configProjectHome :: FilePath
  -- ^ The home dir for this project, absolute path required.
  , _configProjectDirs :: [FilePath]
  -- ^ The subdirs for this project.
  } deriving (Eq, Show)

makeLenses ''Config
makeLenses ''ConfigProject

defaultConfig :: Config
defaultConfig = Config []

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \ obj -> ConfigProject
    <$> obj .: "name"
    <*> (decodeString <$> obj .: "home")
    <*> map (map decodeString) (obj .: "dirs")

instance FromJSON Config where
  parseJSON = withArray "Config" $ \ arr ->
    Config . toList <$> traverse parseJSON arr
