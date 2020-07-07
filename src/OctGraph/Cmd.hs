module OctGraph.Cmd where

import           RIO
import qualified RIO.Text          as T

import           Data.Fallible
import qualified Mix.Plugin.Logger as MixLogger
import           OctGraph.Env
import           OctGraph.Pulls

cmd :: RIO Env ()
cmd = evalContT $ do
  repos <- asks (view #repositories . view #config)
  forM_ repos $ \repo -> do
    let (owner, name) = splitRepoName repo
    pulls <- lift (fetchAllPulls owner name) !?= err
    MixLogger.logInfo (display $ "   all pulls: " <> tshow (length pulls))
    MixLogger.logInfo (display $ "closed pulls: " <> tshow (length $ filter isClosed pulls))
  where
    err txt = exit $ MixLogger.logError (display $ repo <> txt)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"

splitRepoName :: Text -> (Text, Text)
splitRepoName repo =
  case T.split (== '/') repo of
    [owner, name] -> (owner, name)
    _             -> ("", "")
