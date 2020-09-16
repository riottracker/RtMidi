{-# LANGUAGE DeriveAnyClass #-}

module Sound.RtMidi.Report
  ( ApiReport (..)
  , Report (..)
  , buildReport
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.IO.Class (MonadIO)
import GHC.Generics (Generic)
import Sound.RtMidi

data ApiReport = ApiReport
  { apiRepApi :: !Api
  , apiRepName :: !String
  , apiRepDisplayName :: !String
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

newtype Report = Report
  { apiReports :: [ApiReport]
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

buildApiReport :: MonadIO m => Api -> m ApiReport
buildApiReport api = do
  name <- apiName api
  displayName <- apiDisplayName api
  pure (ApiReport api name displayName)

buildReport :: MonadIO m => m Report
buildReport = do
  apis <- compiledApis
  apiReps <- traverse buildApiReport apis
  pure (Report apiReps)
