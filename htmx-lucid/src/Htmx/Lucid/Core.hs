{- |
Module      : Htmx.Lucid.Core
Description : Provides core htmx tags

This module defines the "core" 11 HTMX attributes
<https://htmx.org/reference/#attributes>
-}
module Htmx.Lucid.Core where

import Data.Text (Text, pack)
import Htmx.Event
import Htmx.Render
import Htmx.Swap (Swap)
import Lucid (Html, HtmlT, script_, src_)
import Lucid.Base (Attributes, makeAttributes)

-- | <https://htmx.org/attributes/hx-get/>
-- issues a GET to the specified URL
hxGet_ :: Text -> Attributes
hxGet_ = makeAttributes "hx-get"

-- | <https://htmx.org/attributes/hx-get/>
-- issues a POST to the specified URL
hxPost_ :: Text -> Attributes
hxPost_ = makeAttributes "hx-post"

-- | <https://htmx.org/attributes/hx-push-url/>
-- push a URL into the browser location bar to create history
hxPushUrl_ :: Text -> Attributes
hxPushUrl_ = makeAttributes "hx-push-url"

-- | <https://htmx.org/attributes/hx-select/>
-- select content to swap in from a response
hxSelect_ :: Text -> Attributes
hxSelect_ = makeAttributes "hx-select"

-- | <https://htmx.org/attributes/hx-select-oob/>
-- select content to swap in from a response, somewhere other than the target
-- (out of band)
hxSelectOob_ :: Text -> Attributes
hxSelectOob_ = makeAttributes "hx-select-oob"

-- | <https://htmx.org/attributes/hx-swap/>
-- controls how content will swap in (outerHTML, beforeend, afterend, …)
hxSwap_ :: Text -> Attributes
hxSwap_ = makeAttributes "hx-swap"

-- | Like 'hxSwap_' but takes a strongly typed swap style.
-- This doesn't allow [modifiers](https://htmx.org/attributes/hx-swap/#modifiers) to be applied.
hxSwapS_ :: Swap -> Attributes
hxSwapS_ = makeAttributes "hx-swap" . render

-- | <https://htmx.org/attributes/hx-swap-oob/>
-- mark element to swap in from a response (out of band)
hxSwapOob_ :: Text -> Attributes
hxSwapOob_ = makeAttributes "hx-swap-oob"

-- | <https://htmx.org/attributes/hx-target/>
-- specifies the target element to be swapped
hxTarget_ :: Text -> Attributes
hxTarget_ = makeAttributes "hx-target"

-- | <https://htmx.org/attributes/hx-trigger/>
-- specifies the event that triggers the request
hxTrigger_ :: Text -> Attributes
hxTrigger_ = makeAttributes "hx-trigger"

-- | <https://htmx.org/attributes/hx-vals/>
-- add values to submit with the request (JSON format)
hxVals_ :: Text -> Attributes
hxVals_ = makeAttributes "hx-vals"

-- | Indicates whether you are handling an arbitrary DOM event
-- or on of the bespoke 'HtmxEvent' (defined by the htmx js bundle)
data OnEvent = DomOnEvent Text | HtmxOnEvent HtmxEvent

-- | <https://htmx.org/attributes/hx-on/>
-- handle events with inline scripts on elements
hxOn_ :: OnEvent -> Text -> Attributes
hxOn_ = \case
    DomOnEvent event -> makeAttributes $ "hx-on:" <> event
    HtmxOnEvent htmxEvent -> makeAttributes $ "hx-on::" <> render htmxEvent
