module Types where

import Prelude

import Data.Aeson ((.:), FromJSON, parseJSON, withObject)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map

data Config = Config
  { configProjects :: Map Text ConfigProject
  -- ^ The list of projects.
  } deriving (Eq, Show)

data ConfigProject = ConfigProject
  { configProjectHome :: FilePath
  -- ^ The home dir for this project, absolute path required.
  , configProjectDirs :: [FilePath]
  -- ^ The subdirs for this project.
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config mempty

instance FromJSON ConfigProject where
  parseJSON = withObject "ConfigProject" $ \obj -> ConfigProject
    <$> obj .: "home"
    <*> obj .: "dirs"

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    Config . Map.fromList . HashMap.toList <$> traverse parseJSON obj
