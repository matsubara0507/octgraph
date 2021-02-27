module OctGraph.Cmd where

import           RIO
import qualified RIO.Map               as Map

import           Data.Fallible
import qualified Mix.Plugin.Logger     as MixLogger
import           OctGraph.Cache        (readCache, writeCache)
import           OctGraph.Cmd.Chart
import           OctGraph.Config
import           OctGraph.Env
import           OctGraph.Pulls        as Pulls
import           OctGraph.Pulls.Review as Review

data Cmd
    = PullRequestFrequency
    | ReviewFrequency
    deriving (Show, Eq)

run :: Cmd -> RIO Env ()
run cmd = do
  ps <- fetchPulls =<< asks (view #repositories . view #config)
  case cmd of
    PullRequestFrequency ->
      createPullRequestFrequency (concat $ Map.elems ps)
    ReviewFrequency -> do
      rs <- fetchReviews ps
      createReviewFrequency (concatMap (view #reviews) $ concatMap Map.elems $ Map.elems rs)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"

fetchPulls :: [RepositoryPath] -> RIO Env PullRequests
fetchPulls repos = fmap Map.fromList . forM repos $ \repo -> do
    pulls <- fetchPullsWithCache repo
    MixLogger.logInfo (display repo)
    MixLogger.logInfo (display $ "     all pulls: " <> tshow (length pulls))
    MixLogger.logInfo (display $ "  closed pulls: " <> tshow (length $ filter isClosed pulls))
    pure (repo, pulls)

fetchPullsWithCache :: RepositoryPath -> RIO Env [PullRequest]
fetchPullsWithCache repo = evalContT $ do
  path <- lift $ Pulls.cachePath repo
  MixLogger.logDebug (fromString $ "read cache: " <> path)
  cachedPulls <- lift $ readCache path
  pulls <- lift (fetchPullsWith cachedPulls) !?= err
  when (length pulls /= length cachedPulls) $ do
    MixLogger.logDebug (fromString $ "write cache: " <> path)
    lift (writeCache path pulls)
  pure pulls
  where
    err txt = exit $ MixLogger.logError (display $ repo <> txt) >> pure []

    fetchPullsWith []     = fetchAllPulls repo
    fetchPullsWith cached = fmap (`mergePulls` cached) <$> fetchLatestPulls repo

fetchReviews :: PullRequests -> RIO Env (Map RepositoryPath (Map Int PullRequestReviews))
fetchReviews ps = fmap Map.fromList . forM (Map.toList ps) $ \(repo, pulls) ->do
  rs <- fetchReviewsWithCache repo pulls
  MixLogger.logInfo (display repo)
  MixLogger.logInfo (display $ "  all reviews: " <> tshow (sum $ fmap (length . view #reviews) rs))
  pure (repo, rs)

fetchReviewsWithCache :: RepositoryPath -> [PullRequest] -> RIO Env (Map Int PullRequestReviews)
fetchReviewsWithCache repo pulls = evalContT $ do
  path <- lift $ Review.cachePath repo
  MixLogger.logDebug (fromString $ "read cache: " <> path)
  cachedReviews <- lift $ readCache path
  rs <- fmap Map.fromList $ forM (take 100 $ reverse pulls) $ \pull ->
    lift (fetchReviewsWith cachedReviews pull) !?= err
  MixLogger.logDebug (fromString $ "write cache: " <> path)
  lift (writeCache path rs)
  pure rs
  where
    err txt = exit $ MixLogger.logError (display $ repo <> txt) >> pure mempty

    fetchReviewsWith :: Map Int PullRequestReviews -> PullRequest -> RIO Env (Either Text (Int, PullRequestReviews))
    fetchReviewsWith cache pull = fmap (pull ^. #id,) <$>
      case Map.lookup (pull ^. #id) cache of
        Nothing     -> do
          threadDelay 1_000_000
          fmap (toPullRequestReviews pull) <$> Review.fetchAllReviews repo pull
        Just cached ->
          if | Pulls.isClosed pull ->
                  pure $ Right cached
             | pull ^. #updated_at > cached ^. #updated_at -> do
                  threadDelay 1_000_000
                  fmap (toPullRequestReviews pull . mergeReviews (cached ^. #reviews)) <$> Review.fetchLatestReviews repo pull
             | otherwise ->
                  pure $ Right cached
