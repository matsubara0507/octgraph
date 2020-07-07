module OctGraph.Pulls where

import           RIO
import           RIO.Directory     (createDirectoryIfMissing, doesFileExist)
import           RIO.FilePath      (dropFileName, (<.>), (</>))
import qualified RIO.List          as L
import qualified RIO.Text          as T
import           RIO.Time
import qualified RIO.Vector        as V

import qualified Data.Aeson        as J
import           Data.Extensible
import qualified GitHub
import qualified Mix.Plugin.GitHub as MixGitHub
import           OctGraph.Config   (RepositoryPath, splitRepoName)
import           OctGraph.Env

type PullRequest = Record
  '[ "id"         >: Int
   , "created_at" >: UTCTime
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
  resp <- MixGitHub.fetch $ GitHub.pullRequestsForR
    (GitHub.mkName Proxy org)
    (GitHub.mkName Proxy name)
    (GitHub.sortByCreated <> GitHub.stateAll)
    count
  pure $ case resp of
    Left _      -> Left "cannot fetch pulls"
    Right pulls -> Right (fmap toPullRequest $ V.toList pulls)
  where
    (org, name) = splitRepoName repo

toPullRequest :: GitHub.SimplePullRequest -> PullRequest
toPullRequest pull
    = #id         @= (GitHub.unIssueNumber $ GitHub.simplePullRequestNumber pull)
   <: #created_at @= (GitHub.simplePullRequestCreatedAt pull)
   <: #closed_at  @= (GitHub.simplePullRequestClosedAt pull)
   <: nil

isClosed :: PullRequest -> Bool
isClosed = isJust . view #closed_at


mergePulls :: [PullRequest] -> [PullRequest] -> [PullRequest]
mergePulls pulls =
  L.sortBy (\x y -> (x ^. #id) `compare` (y ^. #id))
    . L.nubBy (\x y -> x ^. #id == y ^. #id)
    . (pulls ++)

writeCache :: RepositoryPath -> [PullRequest] -> RIO Env ()
writeCache repo pulls = do
  path <- cachePath repo
  createDirectoryIfMissing True (dropFileName path)
  liftIO $ J.encodeFile path pulls

readCache :: RepositoryPath -> RIO Env [PullRequest]
readCache repo = do
  path    <- cachePath repo
  isExist <- doesFileExist path
  if isExist then
    fromMaybe [] <$> liftIO (J.decodeFileStrict path)
  else
    pure []

cachePath :: RepositoryPath -> RIO Env FilePath
cachePath repo = (</> "pulls" </> T.unpack repo <.> "json") <$> asks (view #cache)
