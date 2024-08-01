{- |
Module      : Htmx.Servant.RequestHeaders
Description : Helper types for HTMX request headers

<https://htmx.org/reference/#request_headers>
-}
module Htmx.Servant.Request where

import Data.Text (Text)
import Servant.API.Header (Header)

-- | indicates that the request is via an element using hx-boost
type HXBoosted = Header "HX-Boosted" Bool

-- | the current URL of the browser
type HXCurrentURL = Header "HX-Current-URL" Text

-- | “true” if the request is for history restoration after a miss in the local history cache
type HXHistoryRestoreRequest = Header "HX-History-Restore-Request" Bool

-- | the user response to an hx-prompt
type HXPrompt a = Header "HX-Prompt" a

-- | always “true”
type HXRequest = Header "HX-Prompt" Bool

-- | the id of the target element if it exists
type HXTarget = Header "HX-Target" Text

-- | the name of the triggered element if it exists
type HXTriggerName = Header "HX-Trigger-Name" Text

-- | the id of the triggered element if it exists
type HXTrigger = Header "HX-Trigger" Text
