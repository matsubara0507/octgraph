module OctGraph.Env where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub as MixGitHub
import           OctGraph.Config

type Env = Record
  '[ "logger" >: LogFunc
   , "github" >: MixGitHub.Token
   , "config" >: Config
   , "cache"  >: FilePath
   , "output" >: Maybe FilePath
   ]
