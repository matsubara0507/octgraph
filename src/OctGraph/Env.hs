module OctGraph.Env where

import           RIO

import           Data.Extensible
import qualified Mix.Plugin.GitHub as MixGitHub

type Env = Record
  '[ "logger" >: LogFunc
   , "github" >: MixGitHub.Token
   ]
