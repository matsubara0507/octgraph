module OctGraph.Cache where

import           RIO
import           RIO.Directory (createDirectoryIfMissing, doesFileExist)
import           RIO.FilePath  (dropFileName)

import qualified Data.Aeson    as J

writeCache :: (MonadIO m, J.ToJSON a) => FilePath -> a -> m ()
writeCache path target = do
  createDirectoryIfMissing True (dropFileName path)
  liftIO $ J.encodeFile path target

readCache :: (MonadIO m, J.FromJSON a, Monoid a) => FilePath  -> m a
readCache path = do
  isExist <- doesFileExist path
  if isExist then
    fromMaybe mempty <$> liftIO (J.decodeFileStrict path)
  else
    pure mempty
