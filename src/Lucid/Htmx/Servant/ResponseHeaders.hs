{-# LANGUAGE DataKinds #-}
{- |
Module      : Lucid.Htmx.Servant.ResponseHeaders
Description : Helper types for HTMX response headers

<https://htmx.org/reference/#response_headers>
-}
module Lucid.Htmx.Servant.RequestHeaders where

import Servant.API.Header (Header)
import Data.Text (Text)
import Lucid.Htmx.Swap (Swap)

-- | allows you to do a client-side redirect that does not do a full page reload
type HXLocation = Header "HX-Location" Text

-- | pushes a new url into the history stack
type HXPushURL = Header "HX-Push-Url" Text

-- | can be used to do a client-side redirect to a new location
type HXRedirect = Header "HX-Redirect" Text

-- | if set to “true” the client-side will do a full refresh of the page
type HXRefresh = Header "HX-Refresh" Bool

-- | replaces the current URL in the location bar
type HXReplaceUrl = Header "HX-Replace-Url" Bool

-- | replaces the current URL in the location bar
type HXReswap = Header "HX-Reswap" Swap

-- | a CSS selector that updates the target of the 
-- content update to a different element on the page
type HXRetarget = Header "HX-Retarget" Text

-- | a CSS selector that allows you to choose which part of the response is used
-- to be swapped in. Overrides an existing hx-select on the triggering element
type HXReselect = Header "HX-Reselect" Text

-- | allows you to trigger client-side events
type HXTrigger = Header "HX-Trigger" Text 

-- | allows you to trigger client-side events after the settle step
type HXTriggerAfterSettle = Header "HX-Trigger-After-Settle" Text

-- | allows you to trigger client-side events after the swap stepallows you to
-- trigger client-side events after the settle step
type HXTriggerAfterSwap = Header "HX-Trigger-After-Swap" Text
