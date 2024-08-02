{- |
Module      : Htmx.Lucid.Extra
Description : Provides extra htmx tags

This module defines additional attributes that can be used to get additional
behaviour
<https://htmx.org/reference/#attributes-additional>
-}
module Htmx.Lucid.Extra where

import Data.Foldable
import Data.List (intersperse)
import Data.Text (Text, pack)
import Htmx.Extension
import Htmx.Render
import Lucid (Html, HtmlT, script_, src_)
import Lucid.Base (Attributes, makeAttributes)

-- | <https://htmx.org/attributes/hx-boost/>
-- add progressive enhancement for links and forms
hxBoost_ :: Text -> Attributes
hxBoost_ = makeAttributes "hx-boost"

-- | <https://htmx.org/attributes/hx-confirm/>
-- shows a confirm() dialog before issuing a request
hxConfirm_ :: Text -> Attributes
hxConfirm_ = makeAttributes "hx-confirm"

-- | <https://htmx.org/attributes/hx-delete/>
-- issues a DELETE to the specified URL
hxDelete_ :: Text -> Attributes
hxDelete_ = makeAttributes "hx-delete"

-- | <https://htmx.org/attributes/hx-disable/>
-- disables htmx processing for the given node and any children nodes
hxDisable_ :: Attributes
hxDisable_ = makeAttributes "hx-disable" mempty

-- | <https://htmx.org/attributes/hx-disabled-elt/>
-- adds the disabled attribute to the specified elements while a request is in flight
hxDisabledElt_ :: Text -> Attributes
hxDisabledElt_ = makeAttributes "hx-disabled-elt"

-- | <https://htmx.org/attributes/hx-disinherit/>
-- control and disable automatic attribute inheritance for child nodes
hxDisinherit_ :: Text -> Attributes
hxDisinherit_ = makeAttributes "hx-disinherit"

-- | <https://htmx.org/attributes/hx-encoding/>
-- changes the request encoding type
hxEncoding_ :: Text -> Attributes
hxEncoding_ = makeAttributes "hx-encoding"

-- | <https://htmx.org/attributes/hx-ext/>
-- extensions to use for this element
hxExt_ :: Text -> Attributes
hxExt_ = makeAttributes "hx-ext"

-- | A typesafe version of 'hxExt_' that works with the "included" extensions
-- that the htmx codebase is tested against
hxExtension_ :: HtmxExtension -> Attributes
hxExtension_ = makeAttributes "hx-ext" . render

-- | Include multiple extensions in one declaration
hxExtensions_ :: [HtmxExtension] -> Attributes
hxExtensions_ = makeAttributes "hx-ext" . fold . intersperse "," . fmap render

-- | <https://htmx.org/attributes/hx-headers/>
-- adds to the headers that will be submitted with the request
hxHeaders_ :: Text -> Attributes
hxHeaders_ = makeAttributes "hx-headers"

-- | <https://htmx.org/attributes/hx-history/>
-- prevent sensitive data being saved to the history cache
hxHistory_ :: Text -> Attributes
hxHistory_ = makeAttributes "hx-history"

-- | <https://htmx.org/attributes/hx-history-elt/>
-- the element to snapshot and restore during history navigation
hxHistoryElt_ :: Attributes
hxHistoryElt_ = makeAttributes "hx-history-elt" mempty

-- | <https://htmx.org/attributes/hx-include/>
-- include additional data in requests
hxInclude_ :: Text -> Attributes
hxInclude_ = makeAttributes "hx-include"

-- | <https://htmx.org/attributes/hx-indicator/>
-- the element to put the htmx-request class on during the request
hxIndicator_ :: Text -> Attributes
hxIndicator_ = makeAttributes "hx-indicator"

-- | An enumeration of the filter types based on the documentation here:
-- <https://htmx.org/attributes/hx-params/>
data ParamsFilter
    = -- | Include all parameters (default)
      All
    | -- | Include no parameters
      None
    | -- | Include all except the list of parameter names
      Exclude [Text]
    | -- | Include all the list of parameter names
      Include [Text]

-- | <https://htmx.org/attributes/hx-params/>
-- filters the parameters that will be submitted with a request
hxParams_ :: ParamsFilter -> Attributes
hxParams_ = \case
    All -> makeAttributes "hx-params" "*"
    None -> makeAttributes "hx-params" "none"
    Exclude ps -> makeAttributes "hx-params" $ "not " <> (fold . intersperse "," $ ps)
    Include ps -> makeAttributes "hx-params" $ fold . intersperse "," $ ps

-- | <https://htmx.org/attributes/hx-patch/>
-- issues a PATCH to the specified URL
hxPatch_ :: Text -> Attributes
hxPatch_ = makeAttributes "hx-patch"

-- | <https://htmx.org/attributes/hx-preserve/>
-- specifies elements to keep unchanged between requests
hxPreserve_ :: Attributes
hxPreserve_ = makeAttributes "hx-preserve" mempty

-- | <https://htmx.org/attributes/hx-prompt/>
-- shows a prompt() before submitting a request
hxPrompt_ :: Text -> Attributes
hxPrompt_ = makeAttributes "hx-prompt"

-- | <https://htmx.org/attributes/hx-put/>
-- issues a PUT to the specified URL
hxPut_ :: Text -> Attributes
hxPut_ = makeAttributes "hx-put"

-- | <https://htmx.org/attributes/hx-replace-url/>
-- replace the URL in the browser location bar
hxReplaceUrl_ :: Text -> Attributes
hxReplaceUrl_ = makeAttributes "hx-replace-url"

-- | <https://htmx.org/attributes/hx-request/>
-- configures various aspects of the request
hxRequest_ :: Text -> Attributes
hxRequest_ = makeAttributes "hx-request"

{-# DEPRECATED
    hxSse_
    "Don't use hx-sse directly, please use the server sent events extension instead https://htmx.org/extensions/server-sent-events/"
    #-}

-- | <https://htmx.org/attributes/hx-sse/>
-- has been moved to an extension. Documentation for older versions
hxSse_ :: Text -> Attributes
hxSse_ = makeAttributes "hx-sse"

-- | An enumeration of the sync strategies based on the documentation here:
-- <https://htmx.org/attributes/hx-sync/>
data SyncStrategy
    = -- | drop (ignore) this request if an existing request is in flight (the default)
      SyncDrop
    | -- | drop (ignore) this request if an existing request is in flight, and, if
      -- that is not the case, abort this request if another request occurs while it is
      -- still in flight
      SyncAbort
    | -- | abort the current request, if any, and replace it with this request
      SyncReplace
    | -- | queue the first request to show up while a request is in flight
      SyncQueueFirst
    | -- | queue the last request to show up while a request is in flight
      SyncQueueLast
    | -- | queue all requests that show up while a request is in flight
      SyncQueueAll

-- | <https://htmx.org/attributes/hx-sync/>
-- control how requests made by different elements are synchronized
hxSync_ :: Text -> Attributes
hxSync_ = makeAttributes "hx-sync"

-- | <https://htmx.org/attributes/hx-sync/>
-- the same as 'hxSync_' but accepts a strongly typed htmx 'SyncStrategy'
hxSyncStrategy_ :: Text -> SyncStrategy -> Attributes
hxSyncStrategy_ selector = \case
    SyncDrop -> makeAttributes "hx-sync" $ selector <> ":" <> "drop"
    SyncAbort -> makeAttributes "hx-sync" $ selector <> ":" <> "abort"
    SyncReplace -> makeAttributes "hx-sync" $ selector <> ":" <> "replace"
    SyncQueueFirst -> makeAttributes "hx-sync" $ selector <> ":" <> "queue first"
    SyncQueueLast -> makeAttributes "hx-sync" $ selector <> ":" <> "queue last"
    SyncQueueAll -> makeAttributes "hx-sync" $ selector <> ":" <> "queue all"

-- | <https://htmx.org/attributes/hx-validate/>
-- force elements to validate themselves before a request
hxValidate_ :: Text -> Attributes
hxValidate_ = makeAttributes "hx-validate"

-- | <https://htmx.org/attributes/hx-vars/>
-- adds values dynamically to the parameters to submit with the request (deprecated, please use hx-vals)
hxVars_ :: Text -> Attributes
hxVars_ = makeAttributes "hx-vars"

{-# DEPRECATED
    hxWs_
    "Don't use hx-ws directly, please use the web sockets extension instead https://htmx.org/extensions/server-sent-events/https://htmx.org/extensions/web-sockets/"
    #-}

-- | <https://htmx.org/attributes/hx-ws/>
-- has been moved to an extension. Documentation for older versions
hxWs_ :: Text -> Attributes
hxWs_ = makeAttributes "hx-ws"
