module OctGraph.Env where

import           RIO
import           RIO.Time

import           Data.Extensible
import           Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import           Graphics.Rendering.Chart.Easy             (Default, EC,
                                                            ToRenderable, def)
import qualified Mix.Plugin.GitHub                         as MixGitHub
import qualified Mix.Plugin.Logger                         as MixLogger
import           OctGraph.Config

type Env = Record
  '[ "logger" >: LogFunc
   , "github" >: MixGitHub.Token
   , "config" >: Config
   , "cache"  >: FilePath
   , "output" >: Maybe FilePath
   , "filter" >: Filter
   ]

createOutputFileWith :: (Default r, ToRenderable r) => EC r () -> RIO Env ()
createOutputFileWith f = asks (view #output) >>= \case
  Nothing   -> pure ()
  Just path -> do
    liftIO $ toFile def path f
    MixLogger.logInfo $ fromString ("output: " <> path)

type Filter = Record
  '[ "start" >: Maybe UTCTime
   , "end"   >: Maybe UTCTime
   ]

toFilter :: Record '[ "start" >: Maybe String, "end" >: Maybe String ] -> Filter
toFilter opts
    = #start @= (parseTimeM True defaultTimeLocale "%Y%m%d" =<< opts ^. #start)
   <: #end   @= (parseTimeM True defaultTimeLocale "%Y%m%d" =<< opts ^. #end)
   <: nil
