-- TODO: Same style as others please
module GitHub.Endpoints.Checks.CheckRuns
    ( createCheckRun
    , createCheckRunR
    , updateCheckRun
    , updateCheckRunR
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

createCheckRun :: Auth -> Name Owner -> Name Repo -> NewCheckRun -> IO (Either Error CheckRun)
createCheckRun auth owner repo  =
    executeRequest auth . createCheckRunR owner repo

createCheckRunR :: Name Owner -> Name Repo -> NewCheckRun -> GenRequest 'MtAntiopePreview 'RW CheckRun
createCheckRunR owner repo =
    Command Post ["repos", toPathPart owner, toPathPart repo, "check-runs"] . encode

updateCheckRun :: Auth -> Name Owner -> Name Repo -> Id CheckRun -> EditCheckRun -> IO (Either Error CheckRun)
updateCheckRun auth owner repo crid  =
    executeRequest auth . updateCheckRunR owner repo crid

updateCheckRunR :: Name Owner -> Name Repo -> Id CheckRun -> EditCheckRun -> GenRequest 'MtAntiopePreview 'RW CheckRun
updateCheckRunR owner repo crid =
    Command Patch ["repos", toPathPart owner, toPathPart repo, "check-runs", toPathPart crid] . encode
