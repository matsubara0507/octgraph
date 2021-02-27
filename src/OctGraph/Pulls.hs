module OctGraph.Pulls where

import           RIO
import           RIO.FilePath      ((<.>), (</>))
import qualified RIO.List          as L
import qualified RIO.Text          as T
import           RIO.Time
import qualified RIO.Vector        as V

import           Data.Extensible
import qualified GitHub
import qualified Mix.Plugin.GitHub as MixGitHub
import qualified Mix.Plugin.Logger as MixLogger
import           OctGraph.Config   (RepositoryPath, splitRepoName)
import           OctGraph.Env

type PullRequest = Record
  '[ "id"         >: Int
   , "created_at" >: UTCTime
   , "updated_at" >: UTCTime
   , "closed_at"  >: Maybe UTCTime
   ]

type PullRequests = Map RepositoryPath [PullRequest]

fetchLatestPulls :: RepositoryPath -> RIO Env (Either Text [PullRequest])
fetchLatestPulls = fetchPulls' (GitHub.FetchAtLeast 100)

fetchAllPulls :: RepositoryPath -> RIO Env (Either Text [PullRequest])
fetchAllPulls = fetchPulls' GitHub.FetchAll

fetchPulls' ::
  GitHub.FetchCount -> RepositoryPath -> RIO Env (Either Text [PullRequest])
fetchPulls' count repo = do
  MixLogger.logDebug (display $ "fetch pulls: " <> repo <> " (" <> tshow count <> ")")
  resp <- MixGitHub.fetch $ GitHub.pullRequestsForR
    (GitHub.mkName Proxy org)
    (GitHub.mkName Proxy name)
    (GitHub.sortByCreated <> GitHub.stateAll)
    count
  pure $ case resp of
    Left _      -> Left "cannot fetch pulls"
    Right pulls -> Right (toPullRequest <$> V.toList pulls)
  where
    (org, name) = splitRepoName repo

toPullRequest :: GitHub.SimplePullRequest -> PullRequest
toPullRequest pull
    = #id         @= GitHub.unIssueNumber (GitHub.simplePullRequestNumber pull)
   <: #created_at @= GitHub.simplePullRequestCreatedAt pull
   <: #updated_at @= GitHub.simplePullRequestUpdatedAt pull
   <: #closed_at  @= GitHub.simplePullRequestClosedAt pull
   <: nil

isClosed :: PullRequest -> Bool
isClosed = isJust . view #closed_at

mergePulls :: [PullRequest] -> [PullRequest] -> [PullRequest]
mergePulls pulls =
  L.sortBy (\x y -> (x ^. #id) `compare` (y ^. #id))
    . L.nubBy (\x y -> x ^. #id == y ^. #id)
    . (pulls ++)

cachePath :: RepositoryPath -> RIO Env FilePath
cachePath repo = (</> "pulls" </> T.unpack repo <.> "json") <$> asks (view #cache)
