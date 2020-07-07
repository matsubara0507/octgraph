module OctGraph.Cmd.Chart where

import           RIO
import qualified RIO.Map                                as Map
import           RIO.Time

import           Graphics.Rendering.Chart.Backend.Cairo
import           Graphics.Rendering.Chart.Easy          hiding (view, (^.))
import qualified Mix.Plugin.Logger                      as MixLogger
import           OctGraph.Env
import           OctGraph.Pulls

createChartFile :: [PullRequest] -> RIO Env ()
createChartFile pulls = asks (view #output) >>= \case
  Nothing   -> pure ()
  Just path -> do
    liftIO $ toFile def path $ do
      layout_title .= "PullRequests Open Frequency"
      plot (line "open" [[ (d, length ps) | (d, ps) <- Map.toList pulls' ]])
    MixLogger.logInfo $ fromString ("output: " <> path)
  where
    pulls' :: Map Day [PullRequest]
    pulls' = Map.fromListWith (++) $ map (\x -> (toSunday $ x ^. #created_at, [x])) pulls

toDay :: UTCTime -> Day
toDay = parseTimeOrError True defaultTimeLocale "%y%m%d" . formatTime defaultTimeLocale "%y%m%d"

toSunday :: UTCTime -> Day
toSunday t = let d = toDay t in case dayOfWeek d of
  Sunday    -> d
  Monday    -> addDays (-1) d
  Tuesday   -> addDays (-2) d
  Wednesday -> addDays (-3) d
  Thursday  -> addDays (-4) d
  Friday    -> addDays (-5) d
  Saturday  -> addDays (-6) d
