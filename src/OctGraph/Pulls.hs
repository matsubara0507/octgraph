module OctGraph.Pulls where

import           RIO
import           RIO.Time
import qualified RIO.Vector        as V

import           Data.Extensible
import qualified GitHub
import qualified Mix.Plugin.GitHub as MixGitHub
import           OctGraph.Env

type PullRequest = Record
  '[ "id"         >: Int
   , "created_at" >: UTCTime
   , "closed_at"  >: Maybe UTCTime
   ]

fetchAllPulls :: Text -> Text -> RIO Env (Either Text [PullRequest])
fetchAllPulls org repo = do
  resp <- MixGitHub.fetch $ GitHub.pullRequestsForR
    (GitHub.mkName Proxy org)
    (GitHub.mkName Proxy repo)
    (GitHub.sortByCreated <> GitHub.stateAll)
    GitHub.FetchAll
  pure $ case resp of
    Left _      -> Left "cannot fetch pulls"
    Right pulls -> Right (fmap toPullRequest $ V.toList pulls)

toPullRequest :: GitHub.SimplePullRequest -> PullRequest
toPullRequest pull
    = #id         @= (GitHub.unIssueNumber $ GitHub.simplePullRequestNumber pull)
   <: #created_at @= (GitHub.simplePullRequestCreatedAt pull)
   <: #closed_at  @= (GitHub.simplePullRequestClosedAt pull)
   <: nil

isClosed :: PullRequest -> Bool
isClosed = isJust . view #closed_at
