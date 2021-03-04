module OctGraph.Pulls.Review where

import           RIO
import           RIO.FilePath                          ((<.>), (</>))
import qualified RIO.List                              as L
import qualified RIO.Text                              as T
import           RIO.Time
import qualified RIO.Vector                            as V

import           Data.Extensible
import qualified GitHub.Data.Definitions               as GitHub
import qualified GitHub.Data.Name                      as GitHub
import qualified GitHub.Data.Reviews                   as GitHub
import qualified GitHub.Endpoints.PullRequests.Reviews as GitHub
import qualified Mix.Plugin.GitHub                     as MixGitHub
import qualified Mix.Plugin.Logger                     as MixLogger
import           OctGraph.Config                       (RepositoryPath,
                                                        splitRepoName)
import           OctGraph.Env
import           OctGraph.Pulls                        (PullRequest)

type PullRequestReviews = Record
  '[ "id" >: Int
   , "updated_at" >: UTCTime
   , "reviews" >: [Review]
   ]

type Review = Record
   '[ "id" >: Int
    , "pull" >: Int
    , "user" >: Text
    , "created_at" >: UTCTime
    ]

fetchLatestReviews, fetchAllReviews :: RepositoryPath -> PullRequest -> RIO Env (Either Text [Review])
fetchLatestReviews = fetchReviews' (GitHub.FetchAtLeast 100)
fetchAllReviews = fetchReviews' GitHub.FetchAll

fetchReviews' :: GitHub.FetchCount -> RepositoryPath -> PullRequest -> RIO Env (Either Text [Review])
fetchReviews' count repo pr = do
  MixLogger.logDebug (display $ "fetch reviews: " <> repo <> "#" <> tshow (pr ^. #id) <> " (" <> tshow count <> ")")
  resp <- MixGitHub.fetch $ GitHub.pullRequestReviewsR
    (GitHub.mkName Proxy org)
    (GitHub.mkName Proxy name)
    (GitHub.IssueNumber $ pr ^. #id)
    count
  case resp of
    Left err      -> MixLogger.logError (displayShow err) $> Left " cannot fetch reviews"
    Right reviews -> pure $ Right (toReview pr <$> V.toList reviews)
  where
    (org, name) = splitRepoName repo

toReview :: PullRequest -> GitHub.Review -> Review
toReview pr review
    = #id         @= GitHub.untagId (GitHub.reviewId review)
   <: #pull       @= (pr ^. #id)
   <: #user       @= GitHub.untagName (GitHub.simpleUserLogin $ GitHub.reviewUser review)
   <: #created_at @= GitHub.reviewSubmittedAt review
   <: nil

toPullRequestReviews :: PullRequest -> [Review] -> PullRequestReviews
toPullRequestReviews pull reviews
    = #id @= (pull ^. #id)
   <: #updated_at @= (pull ^. #updated_at)
   <: #reviews @= reviews
   <: nil

mergeReviews :: [Review] -> [Review] -> [Review]
mergeReviews reviews =
  L.sortBy (\x y -> (x ^. #id) `compare` (y ^. #id))
    . L.nubBy (\x y -> x ^. #id == y ^. #id)
    . (reviews ++)

cachePath :: RepositoryPath -> PullRequest -> RIO Env FilePath
cachePath repo pr = (</> "reviews" </> T.unpack repo </> show (pr ^. #id) <.> "json") <$> asks (view #cache)
