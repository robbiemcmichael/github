module GitHub.Data.Reviews where

import GitHub.Data.Definitions (SimpleUser)
import GitHub.Data.Id (Id)
import GitHub.Data.PullRequests (SimplePullRequest)
import GitHub.Data.Repos (Repo)
import GitHub.Data.URL (URL)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text as T

data ReviewState
    = ReviewStatePending
    | ReviewStateApproved
    | ReviewStateDismissed
    | ReviewStateCommented
    | ReviewStateChangesRequested
    deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData ReviewState where
    rnf = genericRnf

instance Binary ReviewState

instance FromJSON ReviewState where
    parseJSON = withText "ReviewState" $ \s -> case T.toCaseFold s of
        "approved"          -> pure ReviewStateApproved
        "pending"           -> pure ReviewStatePending
        "approved"          -> pure ReviewStateApproved
        "dismissed"         -> pure ReviewStateDismissed
        "commented"         -> pure ReviewStateCommented
        "changes_requested" -> pure ReviewStateChangesRequested
        _                   -> fail $ "Unexpected ReviewState " <> show s

data Review = Review
    { reviewBody :: !Text
    , reviewCommitId :: !Text
    , reviewState :: ReviewState
    , reviewSubmittedAt :: !UTCTime
    , reviewPullRequestUrl :: !URL
    , reviewHtmlUrl :: !Text
    , reviewUser :: !SimpleUser
    , reviewId :: !(Id Review)
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Review where
    rnf = genericRnf

instance Binary Review

instance FromJSON Review where
    parseJSON =
        withObject "Review" $ \o ->
            Review <$> o .: "body" <*> o .: "commit_id" <*> o .: "state" <*>
            o .: "submitted_at" <*>
            o .: "pull_request_url" <*>
            o .: "html_url" <*>
            o .: "user" <*>
            o .: "id"

data ReviewComment = ReviewComment
    { reviewCommentId :: !(Id ReviewComment)
    , reviewCommentUser :: !SimpleUser
    , reviewCommentBody :: !Text
    , reviewCommentUrl :: !URL
    , reviewCommentPullRequestReviewId :: !(Id Review)
    , reviewCommentDiffHunk :: !Text
    , reviewCommentPath :: !Text
    , reviewCommentPosition :: !Int
    , reviewCommentOriginalPosition :: !Int
    , reviewCommentCommitId :: !Text
    , reviewCommentOriginalCommitId :: !Text
    , reviewCommentCreatedAt :: !UTCTime
    , reviewCommentUpdatedAt :: !UTCTime
    , reviewCommentHtmlUrl :: !URL
    , reviewCommentPullRequestUrl :: !URL
    } deriving (Show, Generic)

instance NFData ReviewComment where
    rnf = genericRnf

instance Binary ReviewComment

instance FromJSON ReviewComment where
    parseJSON =
        withObject "ReviewComment" $ \o -> ReviewComment
            <$> o .: "id"
            <*> o .: "user"
            <*> o .: "body"
            <*> o .: "url"
            <*> o .: "pull_request_review_id"
            <*> o .: "diff_hunk"
            <*> o .: "path"
            <*> o .: "position"
            <*> o .: "original_position"
            <*> o .: "commit_id"
            <*> o .: "original_commit_id"
            <*> o .: "created_at"
            <*> o .: "updated_at"
            <*> o .: "html_url"
            <*> o .: "pull_request_url"

data PullRequestReviewEvent = PullRequestReviewEvent
    { pullRequestReviewEventAction      :: !PullRequestReviewEventType
    , pullRequestReviewEventPullRequest :: !SimplePullRequest
    , pullRequestReviewEventReview      :: !Review
    , pullRequestReviewEventRepository  :: !Repo
    , pullRequestReviewEventSender      :: !SimpleUser
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData PullRequestReviewEvent where rnf = genericRnf
instance Binary PullRequestReviewEvent

instance FromJSON PullRequestReviewEvent where
    parseJSON = withObject "PullRequestReviewEvent" $ \o -> PullRequestReviewEvent
        <$> o .: "action"
        <*> o .: "pull_request"
        <*> o .: "review"
        <*> o .: "repository"
        <*> o .: "sender"

data PullRequestReviewEventType
    = PullRequestReviewSubmitted
    | PullRequestReviewEdited
    | PullRequestReviewDismissed
    deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData PullRequestReviewEventType where rnf = genericRnf
instance Binary PullRequestReviewEventType

instance FromJSON PullRequestReviewEventType where
    parseJSON = withText "PullRequestReviewEventType" $ \s -> case T.toCaseFold s of
        "submitted" -> pure PullRequestReviewSubmitted
        "edited"    -> pure PullRequestReviewEdited
        "dismissed" -> pure PullRequestReviewDismissed
        _           -> fail $ "Unknown PullRequestReviewEventType " <> T.unpack s
