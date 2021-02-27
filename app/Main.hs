module Main where

import           Paths_octgraph         (version)
import           RIO
import           RIO.Directory          (doesFileExist, getHomeDirectory)
import           RIO.FilePath           ((</>))

import           Configuration.Dotenv   (Config (..), defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           GetOpt                 (withGetOpt')
import           Mix
import qualified Mix.Plugin.GitHub      as MixGitHub
import           Mix.Plugin.Logger      as MixLogger
import           OctGraph.Cmd           as OctGraph
import           OctGraph.Config
import           System.Environment     (getEnv)
import qualified Version

main :: IO ()
main = withGetOpt' "[options] (pulls|reviews)" opts $ \r args usage -> do
  homeDir <- getHomeDirectory
  _ <- loadEnvFileIfExist defaultConfig
  _ <- loadEnvFileIfExist $ defaultConfig { configPath = [homeDir <> "/.env"] }
  if | r ^. #help          -> hPutBuilder stdout (fromString usage)
     | r ^. #version       -> hPutBuilder stdout (Version.build version <> "\n")
     | args == ["pulls"]   -> runCmd r PullRequestFreqency
     | args == ["reviews"] -> runCmd r ReviewFrequency
     | otherwise           -> hPutBuilder stdout (fromString usage)
  where
    loadEnvFileIfExist conf =
      whenM (and <$> mapM doesFileExist (configPath conf)) (void $ loadFile conf)
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #work    @= workOpt
        <: #output  @= outputOpt
        <: #config  @= configOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "work"    >: FilePath
   , "output"  >: Maybe FilePath
   , "config"  >: FilePath
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

workOpt :: OptDescr' FilePath
workOpt = fromMaybe ".octgraph" <$> optLastArg ['w'] ["work"] "PATH" "Work directory PATH"

outputOpt :: OptDescr' (Maybe FilePath)
outputOpt = optLastArg ['o'] ["out"] "PATH" "Output png file PATH"

configOpt :: OptDescr' FilePath
configOpt = fromMaybe "./octgraph.yaml" <$> optLastArg ['c'] ["config"] "PATH" "Configuration PATH (default: ./octgraph.yaml"

runCmd :: Options -> Cmd -> IO ()
runCmd opts subcmd = do
  gToken <- liftIO $ fromString <$> getEnv "GH_TOKEN"
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #github <@=> MixGitHub.buildPlugin gToken
            <: #config <@=> readConfig (opts ^. #config)
            <: #cache  <@=> pure (opts ^. #work </> "cache")
            <: #output <@=> pure (opts ^. #output)
            <: nil
  Mix.run plugin $ OctGraph.run subcmd
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
