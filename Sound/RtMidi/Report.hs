{-# LANGUAGE DeriveAnyClass #-}

module Sound.RtMidi.Report
  ( ApiReport (..)
  , Report (..)
  , buildReport
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO)
import Data.List (nub)
import GHC.Generics (Generic)
import Sound.RtMidi

data ApiReport = ApiReport
  { apiRepApi :: !Api
  , apiRepName :: !String
  , apiRepDisplayName :: !String
  , apiInPorts :: ![(Int, String)]
  , apiOutPorts :: ![(Int, String)]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

data Report = Report
  { defaultInApi :: !Api
  , defaultOutApi :: !Api
  , apiReports :: ![ApiReport]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

buildApiReport :: MonadIO m => Api -> m ApiReport
buildApiReport api = do
  name <- apiName api
  displayName <- apiDisplayName api
  inDev <- createInput api ("rtmidi-report-input-" ++ name) 0
  inPorts <- listPorts inDev
  outDev <- createOutput api ("rtmidi-report-output-" ++ name)
  outPorts <- listPorts outDev
  pure (ApiReport api name displayName inPorts outPorts)

buildReport :: MonadIO m => Bool -> m Report
buildReport defaultOnly = do
  inDev <- defaultInput
  defInApi <- currentApi inDev
  outDev <- defaultOutput
  defOutApi <- currentApi outDev
  apis <- if defaultOnly then pure (nub [defInApi, defOutApi]) else compiledApis
  apiReps <- traverse buildApiReport apis
  pure (Report defInApi defOutApi apiReps)