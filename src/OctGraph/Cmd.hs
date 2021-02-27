module OctGraph.Cmd where

import           RIO
import qualified RIO.List              as L
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
    = PullRequestFreqency
    | ReviewFrequency
    deriving (Show, Eq)

run :: Cmd -> RIO Env ()
run cmd = do
  ps <- fetchPulls =<< asks (view #repositories . view #config)
  case cmd of
    PullRequestFreqency ->
      createPullRequestFreqency (concat $ Map.elems ps)
    ReviewFrequency -> do
      rs <- fetchReviews ps
      createReviewFrequency (concat $ concatMap Map.elems $ Map.elems rs)

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

fetchReviews :: PullRequests -> RIO Env (Map RepositoryPath  Reviews)
fetchReviews ps = fmap Map.fromList . forM (Map.toList ps) $ \(repo, pulls) ->do
  reviews <- fetchReviewsWithCache repo pulls
  MixLogger.logInfo (display repo)
  MixLogger.logInfo (display $ "  all reviews: " <> tshow (sum $ fmap length reviews))
  pure (repo, reviews)

fetchReviewsWithCache :: RepositoryPath -> [PullRequest] -> RIO Env Reviews
fetchReviewsWithCache repo pulls = evalContT $ do
  path <- lift $ Review.cachePath repo
  MixLogger.logDebug (fromString $ "read cache: " <> path)
  cachedReviews <- lift $ readCache path
  rs <- fmap Map.fromList $ forM (take 100 $ reverse pulls) $ \pull -> do
    threadDelay 1_000_000
    lift (fetchReviewsWith cachedReviews pull) !?= err
  MixLogger.logDebug (fromString $ "write cache: " <> path)
  lift (writeCache path rs)
  pure rs
  where
    err txt = exit $ MixLogger.logError (display $ repo <> txt) >> pure mempty

    fetchReviewsWith :: Map Int [Review] -> PullRequest -> RIO Env (Either Text (Int, [Review]))
    fetchReviewsWith cache pull = fmap (pull ^. #id,) <$>
      case Map.lookup (pull ^. #id) cache of
        Nothing     -> Review.fetchAllReviews repo pull
        Just cached -> do
          if Pulls.isClosed pull then
            pure $ Right cached
          else
            case L.maximumByMaybe (compare `on` view #created_at) cached of
              Nothing     -> Review.fetchAllReviews repo pull
              Just latest ->
                if pull ^. #updated_at > latest ^. #created_at then
                  fmap (`mergeReviews` cached) <$> Review.fetchLatestReviews repo pull
                else
                  pure $ Right cached
