{-# LANGUAGE DeriveAnyClass #-}

-- | Utility code to gather MIDI system information.
module Sound.RtMidi.Report
  ( ApiReport (..)
  , Report (..)
  , buildApiReport
  , buildCustomReport
  , buildReport
  )
where

import Control.DeepSeq (NFData)
import Data.List (nub)
import GHC.Generics (Generic)
import Sound.RtMidi
  ( Api
  , apiDisplayName
  , apiName
  , compiledApis
  , createInput
  , createOutput
  , currentApi
  , defaultInput
  , defaultOutput
  , listPorts
  )

-- | MIDI system information specific to a particular API.
data ApiReport = ApiReport
  { apiRepApi :: !Api
  , apiRepName :: !String
  , apiRepDisplayName :: !String
  , apiInPorts :: ![(Int, String)]
  , apiOutPorts :: ![(Int, String)]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | MIDI system information for any number of APIs.
data Report = Report
  { defaultInApi :: !Api
  , defaultOutApi :: !Api
  , apiReports :: ![ApiReport]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | Gather information about the given 'Api', including port information.
buildApiReport :: Api -> IO ApiReport
buildApiReport api = do
  name <- apiName api
  displayName <- apiDisplayName api
  inDev <- createInput api ("rtmidi-report-input-" ++ name) 0
  inPorts <- listPorts inDev
  outDev <- createOutput api ("rtmidi-report-output-" ++ name)
  outPorts <- listPorts outDev
  pure (ApiReport api name displayName inPorts outPorts)

-- | Variant of 'buildReport' that allows you to restrict it to the default APIs.
buildCustomReport
  :: Bool
  -- ^ True to report on default APIs, False to report on all compiled APIs.
  -> IO Report
buildCustomReport defaultOnly = do
  inDev <- defaultInput
  defInApi <- currentApi inDev
  outDev <- defaultOutput
  defOutApi <- currentApi outDev
  apis <- if defaultOnly then pure (nub [defInApi, defOutApi]) else compiledApis
  apiReps <- traverse buildApiReport apis
  pure (Report defInApi defOutApi apiReps)

-- | Gather information about all compiled APIs.
buildReport :: IO Report
buildReport = buildCustomReport False
