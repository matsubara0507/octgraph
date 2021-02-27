module OctGraph.Cmd.Chart where

import           RIO
import qualified RIO.Map                       as Map
import           RIO.Time

import           Graphics.Rendering.Chart.Easy hiding (Review, reviews, view,
                                                (^.))
import qualified OctGraph.Config               as Config
import           OctGraph.Env
import           OctGraph.Pulls                (PullRequest)
import           OctGraph.Pulls.Review         (Review)
import           Witherable                    (ordNubOn)

createPullRequestFrequency :: [PullRequest] -> RIO Env ()
createPullRequestFrequency pulls = createOutputFileWith $ do
  layout_title .= "PullRequests Open Frequency"
  plot (line "open" [[ (d, length ps) | (d, ps) <- Map.toList pulls' ]])
  where
    pulls' :: Map Week [PullRequest]
    pulls' = Map.fromListWith (++) $ map (\x -> (toWeek $ x ^. #created_at, [x])) pulls

toDay :: UTCTime -> Day
toDay = parseTimeOrError True defaultTimeLocale "%y%m%d" . formatTime defaultTimeLocale "%y%m%d"

type Week = Day

toWeek :: UTCTime -> Week
toWeek = toWeek' . toDay

toWeek' :: Day -> Week
toWeek' d = case dayOfWeek d of
  Sunday    -> d
  Monday    -> addDays (-1) d
  Tuesday   -> addDays (-2) d
  Wednesday -> addDays (-3) d
  Thursday  -> addDays (-4) d
  Friday    -> addDays (-5) d
  Saturday  -> addDays (-6) d

createReviewFrequency :: [Review] -> RIO Env ()
createReviewFrequency reviews = do
  user <- Config.reviewFilerUser <$> asks (view #config)
  createOutputFileWith $ do
    layout_title .= "Review Frequency"
    plot (line "review" [[ (d, length rs) | (d, rs) <- Map.toList (reviews' user) ]])
  where
    reviews' :: Maybe Text -> Map Day [Review]
    reviews' user =
      Map.mapKeysWith (++) toWeek'
        $ fmap (ordNubOn $ view #pull)
        $ Map.fromListWith (++)
        $ map (\x -> (toDay $ x ^. #created_at, [x]))
        $ maybe id (\u -> filter $ \x -> x ^. #user == u) user reviews
