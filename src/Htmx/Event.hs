{- |
Module      : Htmx.Event
Description : Enumerates htmx specific events

This module defines a type that represents events that originate from the HTMX
library itself
<https://htmx.org/reference/#events>
-}
module Htmx.Event where

import Data.Text (Text)
import Htmx.Render

-- | <https://htmx.org/reference/#events>
-- A sum type that represents possible events originating from the HTMX
-- javascript library
data HtmxEvent
    = -- | send this event to an element to abort a request
      Abort
    | -- | triggered after an AJAX request has completed processing a successful
      -- response
      AfterOnLoad
    | -- | triggered after htmx has initialized a node
      AfterProcessNode
    | -- | triggered after an AJAX request has completed
      AfterRequest
    | -- | triggered after the DOM has settled
      AfterSettle
    | -- | triggered after new content has been swapped in
      AfterSwap
    | -- | triggered before htmx disables an element or removes it from the DOM
      BeforeCleanupElement
    | -- | triggered before any response processing occurs
      BeforeOnLoad
    | -- | triggered before htmx initializes a node
      BeforeProcessNode
    | -- | triggered before an AJAX request is made
      BeforeRequest
    | -- | triggered before a swap is done, allows you to configure the swap
      BeforeSwap
    | -- | triggered just before an ajax request is sent
      BeforeSend
    | -- | triggered before the request, allows you to customize parameters,
      -- headers
      ConfigRequest
    | -- | triggered after a trigger occurs on an element, allows you to cancel
      -- (or delay) issuing the AJAX request
      Confirm
    | -- | triggered on an error during cache writing
      HistoryCacheError
    | -- | triggered on a cache miss in the history subsystem
      HistoryCacheMiss
    | -- | triggered on a unsuccessful remote retrieval
      HistoryCacheMissError
    | -- | triggered on a successful remote retrieval
      HistoryCacheMissLoad
    | -- | triggered when htmx handles a history restoration action
      HistoryRestore
    | -- | triggered before content is saved to the history cache
      BeforeHistorySave
    | -- | triggered when new content is added to the DOM
      Load
    | -- | triggered when an element refers to a SSE event in its trigger, but
      -- no parent SSE source has been defined
      NoSSESourceError
    | -- | triggered when an exception occurs during the onLoad handling in htmx
      OnLoadError
    | -- | triggered after an out of band element as been swapped in
      OobAfterSwap
    | -- | triggered before an out of band element swap is done, allows you to
      -- configure the swap
      OobBeforeSwap
    | -- | triggered when an out of band element does not have a matching ID in
      -- the current DOM
      OobErrorNoTarget
    | -- | triggered after a prompt is shown
      Prompt
    | -- | triggered after an url is pushed into history
      PushedIntoHistory
    | -- | triggered when an HTTP response error (non-200 or 300 response code)
      -- occurs
      ResponseError
    | -- | triggered when a network error prevents an HTTP request from happening
      SendError
    | -- | triggered when an error occurs with a SSE source
      SseError
    | -- | triggered when a SSE source is opened
      SseOpen
    | -- | triggered when an error occurs during the swap phase
      SwapError
    | -- | triggered when an invalid target is specified
      TargetError
    | -- | triggered when a request timeout occurs
      Timeout
    | -- | triggered before an element is validated
      ValidationValidate
    | -- | triggered when an element fails validation
      ValidationFailed
    | -- | triggered when a request is halted due to validation errors
      ValidationHalted
    | -- | triggered when an ajax request aborts
      XhrAbort
    | -- | triggered when an ajax request ends
      XhrLoadend
    | -- | triggered when an ajax request starts
      XhrLoadstart
    | -- | triggered periodically during an ajax request that supports progress
      -- events
      XhrProgress

instance Render HtmxEvent where
    render = \case
        Abort -> "abort"
        AfterOnLoad -> "afterOnLoad"
        AfterProcessNode -> "afterProcessNode"
        AfterRequest -> "afterRequest"
        AfterSettle -> "afterSettle"
        AfterSwap -> "afterSwap"
        BeforeCleanupElement -> "beforeCleanupElement"
        BeforeOnLoad -> "beforeOnLoad"
        BeforeProcessNode -> "beforeProcessNode"
        BeforeRequest -> "beforeRequest"
        BeforeSwap -> "beforeSwap"
        BeforeSend -> "beforeSend"
        ConfigRequest -> "configRequest"
        Confirm -> "confirm"
        HistoryCacheError -> "historyCacheError"
        HistoryCacheMiss -> "historyCacheMiss"
        HistoryCacheMissError -> "historyCacheMissError"
        HistoryCacheMissLoad -> "historyCacheMissLoad"
        HistoryRestore -> "historyRestore"
        BeforeHistorySave -> "beforeHistorySave"
        Load -> "load"
        NoSSESourceError -> "noSSESourceError"
        OnLoadError -> "onLoadError"
        OobAfterSwap -> "oobAfterSwap"
        OobBeforeSwap -> "oobBeforeSwap"
        OobErrorNoTarget -> "oobErrorNoTarget"
        Prompt -> "prompt"
        PushedIntoHistory -> "pushedIntoHistory"
        ResponseError -> "responseError"
        SendError -> "sendError"
        SseError -> "sseError"
        SseOpen -> "sseOpen"
        SwapError -> "swapError"
        TargetError -> "targetError"
        Timeout -> "timeout"
        ValidationValidate -> "validation:validate"
        ValidationFailed -> "validation:failed"
        ValidationHalted -> "validation:halted"
        XhrAbort -> "xhr:abort"
        XhrLoadend -> "xhr:loadend"
        XhrLoadstart -> "xhr:loadstart"
        XhrProgress -> "xhr:progress"
