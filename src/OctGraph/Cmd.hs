module OctGraph.Cmd where

import           RIO

import           Data.Fallible
import qualified Mix.Plugin.Logger as MixLogger
import           OctGraph.Config
import           OctGraph.Env
import           OctGraph.Pulls    as Pulls

cmd :: RIO Env ()
cmd = do
  repos <- asks (view #repositories . view #config)
  forM_ repos $ \repo -> do
    pulls <- fetchPullsWithCache repo
    MixLogger.logInfo (display repo)
    MixLogger.logInfo (display $ "     all pulls: " <> tshow (length pulls))
    MixLogger.logInfo (display $ "  closed pulls: " <> tshow (length $ filter isClosed pulls))

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"

fetchPullsWithCache :: RepositoryPath -> RIO Env [PullRequest]
fetchPullsWithCache repo = evalContT $ do
  cachedPulls <- lift $ Pulls.readCache repo
  pulls <- lift (fetchPullsWith cachedPulls) !?= err
  when (length pulls /= length cachedPulls) $
    lift (Pulls.writeCache repo pulls)
  pure pulls
  where
    err txt = exit $ MixLogger.logError (display $ repo <> txt) >> pure []

    fetchPullsWith []     = fetchAllPulls repo
    fetchPullsWith cached = fmap (flip mergePulls cached) <$> fetchLatestPulls repo
