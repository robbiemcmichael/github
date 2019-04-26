{-# LANGUAGE RecordWildCards #-}
-- TODO: Same style as others please
module GitHub.Data.CheckRuns
    ( CheckRun(..)
    , NewCheckRun(..)
    , EditCheckRun(..)
    , CheckRunStatus(..)
    , CheckRunConclusion(..)
    , CheckRunOutput(..)
    ) where

import GitHub.Data.GitData     (Commit)
import GitHub.Data.Name        (Name)
import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

-- TODO:
-- This module only implements a subset of the real API. It should be improved
-- and submitted as a PR to the github library

data CheckRun = CheckRun
    { checkRunId          :: !(Id CheckRun)
    , checkRunName        :: !Text
    , checkRunHeadSha     :: !(Name Commit)
    -- Not implemented: details_url
    -- Not implemented: external_id
    , checkRunStatus      :: !(Maybe CheckRunStatus)
    , checkRunStartedAt   :: !(Maybe UTCTime)
    , checkRunConclusion  :: !(Maybe CheckRunConclusion)
    , checkRunCompletedAt :: !(Maybe UTCTime)
    , checkRunOutput      :: !(Maybe CheckRunOutput)
    -- Not implemented: actions
    -- Not implemented: check_suite
    -- Not implemented: app
    -- Not implemented: pull_requests
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CheckRun where rnf = genericRnf
instance Binary CheckRun

instance ToJSON CheckRun where
    toJSON CheckRun{..} = object $ filter notNull $
        [ "name"          .= checkRunName
        , "head_sha"      .= checkRunHeadSha
        -- , "details_url"   .= checkRunDetailsUrl
        -- , "external_id"   .= checkRunExternalId
        , "status"        .= checkRunStatus
        , "started_at"    .= checkRunStartedAt
        , "conclusion"    .= checkRunConclusion
        , "completed_at"  .= checkRunCompletedAt
        , "output"        .= checkRunOutput
        -- , "actions"       .= checkRunActions
        -- , "check_suite"   .= checkRunCheckSuite
        -- , "app"           .= checkRunApp
        -- , "pull_requests" .= checkRunPullRequests
        ]
      where
        notNull (_, Null) = False
        notNull (_, _) = True

instance FromJSON CheckRun where
  parseJSON = withObject "CheckRun" $ \o -> CheckRun
      <$> o .:  "id"
      <*> o .:  "name"
      <*> o .:  "head_sha"
      -- <*> o .:? "details_url"
      -- <*> o .:? "external_id"
      <*> o .:? "status"
      <*> o .:? "started_at"
      <*> o .:? "conclusion"
      <*> o .:? "completed_at"
      <*> o .:? "output"
      -- <*> o .:? "actions"
      -- <*> o .:? "check_suite"
      -- <*> o .:? "app"
      -- <*> o .:? "pull_requests"

data NewCheckRun = NewCheckRun
    { newCheckRunName        :: !Text
    , newCheckRunHeadSha     :: !(Name Commit)
    -- Not implemented: details_url
    -- Not implemented: external_id
    , newCheckRunStatus      :: !(Maybe CheckRunStatus)
    , newCheckRunStartedAt   :: !(Maybe UTCTime)
    , newCheckRunConclusion  :: !(Maybe CheckRunConclusion)
    , newCheckRunCompletedAt :: !(Maybe UTCTime)
    , newCheckRunOutput      :: !(Maybe CheckRunOutput)
    -- Not implemented: actions
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewCheckRun where rnf = genericRnf
instance Binary NewCheckRun

instance ToJSON NewCheckRun where
    toJSON NewCheckRun{..} = object $ filter notNull $
        [ "name"         .= newCheckRunName
        , "head_sha"     .= newCheckRunHeadSha
        -- , "details_url"  .= newCheckRunDetailsUrl
        -- , "external_id"  .= newCheckRunExternalId
        , "status"       .= newCheckRunStatus
        , "started_at"   .= newCheckRunStartedAt
        , "conclusion"   .= newCheckRunConclusion
        , "completed_at" .= newCheckRunCompletedAt
        , "output"       .= newCheckRunOutput
        -- , "actions"      .= newCheckRunActions
        ]
      where
        notNull (_, Null) = False
        notNull (_, _) = True

data EditCheckRun = EditCheckRun
    { editCheckRunName        :: !(Maybe Text)
    -- Not implemented: details_url
    -- Not implemented: external_id
    , editCheckRunStatus      :: !(Maybe CheckRunStatus)
    , editCheckRunStartedAt   :: !(Maybe UTCTime)
    , editCheckRunConclusion  :: !(Maybe CheckRunConclusion)
    , editCheckRunCompletedAt :: !(Maybe UTCTime)
    , editCheckRunOutput      :: !(Maybe CheckRunOutput)
    -- Not implemented: actions
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData EditCheckRun where rnf = genericRnf
instance Binary EditCheckRun

instance ToJSON EditCheckRun where
    toJSON EditCheckRun{..} = object $ filter notNull $
        [ "name"         .= editCheckRunName
        -- , "details_url"  .= editCheckRunDetailsUrl
        -- , "external_id"  .= editCheckRunExternalId
        , "status"       .= editCheckRunStatus
        , "started_at"   .= editCheckRunStartedAt
        , "conclusion"   .= editCheckRunConclusion
        , "completed_at" .= editCheckRunCompletedAt
        , "output"       .= editCheckRunOutput
        -- , "actions"      .= editCheckRunActions
        ]
      where
        notNull (_, Null) = False
        notNull (_, _) = True

data CheckRunStatus
    = CheckRunQueued
    | CheckRunInProgress
    | CheckRunCompleted
  deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData CheckRunStatus where rnf = genericRnf
instance Binary CheckRunStatus

instance FromJSON CheckRunStatus where
    parseJSON (String "queued")      = pure CheckRunQueued
    parseJSON (String "in_progress") = pure CheckRunInProgress
    parseJSON (String "completed")   = pure CheckRunCompleted
    parseJSON _ = fail "Could not build a CheckRunStatus"

instance ToJSON CheckRunStatus where
    toJSON CheckRunQueued     = String "queued"
    toJSON CheckRunInProgress = String "in_progress"
    toJSON CheckRunCompleted  = String "completed"

data CheckRunConclusion
    = CheckRunSuccess
    | CheckRunFailure
    | CheckRunNeutral
    | CheckRunCancelled
    | CheckRunTimedOut
    | CheckRunActionRequired
  deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData CheckRunConclusion where rnf = genericRnf
instance Binary CheckRunConclusion

instance FromJSON CheckRunConclusion where
    parseJSON (String "success")         = pure CheckRunSuccess
    parseJSON (String "failure")         = pure CheckRunFailure
    parseJSON (String "neutral")         = pure CheckRunNeutral
    parseJSON (String "cancelled")       = pure CheckRunCancelled
    parseJSON (String "timed_out")       = pure CheckRunTimedOut
    parseJSON (String "action_required") = pure CheckRunActionRequired
    parseJSON _ = fail "Could not build a CheckRunConclusion"

instance ToJSON CheckRunConclusion where
    toJSON CheckRunSuccess        = String "success"
    toJSON CheckRunFailure        = String "failure"
    toJSON CheckRunNeutral        = String "neutral"
    toJSON CheckRunCancelled      = String "cancelled"
    toJSON CheckRunTimedOut       = String "timed_out"
    toJSON CheckRunActionRequired = String "action_required"

data CheckRunOutput = CheckRunOutput
    { checkRunOutputTitle   :: !(Maybe Text)
    , checkRunOutputSummary :: !(Maybe Text)
    , checkRunOutputText    :: !(Maybe Text)
    -- Not implemented: annotations
    -- Not implemented: images
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CheckRunOutput where rnf = genericRnf
instance Binary CheckRunOutput

instance ToJSON CheckRunOutput where
    toJSON (CheckRunOutput t s d) = object $ filter notNull $
        [ "title"       .= t
        , "summary"     .= s
        , "text"        .= d
        -- , "annotations" .= a
        -- , "images"      .= i
        ]
      where
        notNull (_, Null) = False
        notNull (_, _) = True

instance FromJSON CheckRunOutput where
  parseJSON = withObject "CheckRunOutput" $ \o -> CheckRunOutput
      <$> o .:  "title"
      <*> o .:  "summary"
      <*> o .:? "text"
      -- <*> o .:? "annotations"
      -- <*> o .:? "images"
