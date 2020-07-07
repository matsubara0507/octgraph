module OctGraph.Config where

import           RIO
import qualified RIO.Text        as T

import           Data.Extensible
import qualified Data.Yaml       as Y

type Config = Record
  '[ "repositories" >: [RepositoryPath]
   ]

readConfig :: MonadIO m => FilePath -> m Config
readConfig = Y.decodeFileThrow

type RepositoryPath = Text -- owner/repo

splitRepoName :: RepositoryPath -> (Text, Text)
splitRepoName repo =
  case T.split (== '/') repo of
    [owner, name] -> (owner, name)
    _             -> ("", "")
