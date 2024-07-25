{- |
Module      : Lucid.Htmx.Core
Description : Provides core htmx tags

This module defines the "core" 11 HTMX attributes
<https://htmx.org/reference/#attributes>
-}
module Lucid.Htmx.Core where

import Data.Text (Text, pack)
import Lucid (Html, HtmlT, script_, src_)
import Lucid.Base (Attribute, makeAttribute)
import Lucid.Htmx.Event
import Lucid.Htmx.Render

-- | <https://htmx.org/attributes/hx-get/>
-- issues a GET to the specified URL
hxGet_ :: Text -> Attribute
hxGet_ = makeAttribute "hx-get"

-- | <https://htmx.org/attributes/hx-get/>
-- issues a POST to the specified URL
hxPost_ :: Text -> Attribute
hxPost_ = makeAttribute "hx-post"

-- | <https://htmx.org/attributes/hx-push-url/>
-- push a URL into the browser location bar to create history
hxPushUrl_ :: Text -> Attribute
hxPushUrl_ = makeAttribute "hx-push-url"

-- | <https://htmx.org/attributes/hx-select/>
-- select content to swap in from a response
hxSelect_ :: Text -> Attribute
hxSelect_ = makeAttribute "hx-select"

-- | <https://htmx.org/attributes/hx-select-oob/>
-- select content to swap in from a response, somewhere other than the target
-- (out of band)
hxSelectOob_ :: Text -> Attribute
hxSelectOob_ = makeAttribute "hx-select-oob"

-- | <https://htmx.org/attributes/hx-swap/>
-- controls how content will swap in (outerHTML, beforeend, afterend, …)
hxSwap_ :: Text -> Attribute
hxSwap_ = makeAttribute "hx-swap"

-- | <https://htmx.org/attributes/hx-swap-oob/>
-- mark element to swap in from a response (out of band)
hxSwapOob_ :: Text -> Attribute
hxSwapOob_ = makeAttribute "hx-swap-oob"

-- | <https://htmx.org/attributes/hx-target/>
-- specifies the target element to be swapped
hxTarget_ :: Text -> Attribute
hxTarget_ = makeAttribute "hx-target"

-- | <https://htmx.org/attributes/hx-trigger/>
-- specifies the event that triggers the request
hxTrigger_ :: Text -> Attribute
hxTrigger_ = makeAttribute "hx-trigger"

-- | <https://htmx.org/attributes/hx-vals/>
-- add values to submit with the request (JSON format)
hxVals_ :: Text -> Attribute
hxVals_ = makeAttribute "hx-vals"

data OnEvent = DomOnEvent Text | HtmxOnEvent HtmxEvent

-- | <https://htmx.org/attributes/hx-on/>
-- handle events with inline scripts on elements
hxOn_ :: OnEvent -> Text -> Attribute
hxOn_ = \case
    DomOnEvent event -> makeAttribute $ "hx-on:" <> event
    HtmxOnEvent htmxEvent -> makeAttribute $ "hx-on::" <> render htmxEvent
