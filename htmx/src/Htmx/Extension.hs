{- |
Module      : Htmx.Extension
Description : Enumerates official HTMX extensions

This module defines a sum type that represents the "included" HTMX extensions
<https://htmx.org/extensions/#included>
-}
module Htmx.Extension where

import Data.Text (Text)
import Htmx.Render

-- | <https://htmx.org/extensions/>
--
-- htmx includes a set of extensions out of the box that address common
-- developer needs. These extensions are tested against htmx in each distribution.
--
-- You can find the source for the bundled extensions at https://unpkg.com/
-- browse/htmx.org@1.9.12/dist/ext/. You will need to include the javascript file
-- for the extension and then install it using the hx-ext attributes.
-- See the individual extension documentation for more details.
data HtmxExtension
    = -- | <https://htmx.org/extensions/ajax-header/> includes the commonly-used X-Requested-With header that identifies ajax requests in many backend frameworks
      AjaxHeader
    | -- | <https://htmx.org/extensions/alpine-morph/> an extension for using the Alpine.js morph plugin as the swapping mechanism in htmx.
      AlpineMorph
    | -- | <https://htmx.org/extensions/class-tools/> an extension for manipulating timed addition and removal of classes on HTML elements
      ClassTools
    | -- | <https://htmx.org/extensions/client-side-templates/> support for client side template processing of JSON/XML responses
      ClientSideTemplates
    | -- | <https://htmx.org/extensions/debug/> an extension for debugging of a particular element using htmx
      Debug
    | -- | <https://htmx.org/extensions/event-header/> includes a JSON serialized version of the triggering event, if any
      EventHeader
    | -- | <https://htmx.org/extensions/head-support/> support for merging the head tag from responses into the existing documents head
      HeadSupport
    | -- | <https://htmx.org/extensions/include-vals/> allows you to include additional values in a request
      IncludeVals
    | -- | <https://htmx.org/extensions/json-enc/> use JSON encoding in the body of requests, rather than the default x-www-form-urlencoded
      JsonEnc
    | -- | <https://htmx.org/extensions/idiomorph/> an extension for using the idiomorph morphing algorithm as a swapping mechanism
      Idiomorph
    | -- | <https://htmx.org/extensions/loding-states/> allows you to disable inputs, add and remove CSS classes to any element while a request is in-flight.
      LoadingStates
    | -- | <https://htmx.org/extensions/method-override/> use the X-HTTP-Method-Override header for non-GET and POST requests
      MethodOverride
    | -- | <https://htmx.org/extensions/morphdom-swap/> an extension for using the morphdom library as the swapping mechanism in htmx.
      MorphdomSwap
    | -- | <https://htmx.org/extensions/multi-swap/> allows to swap multiple elements with different swap methods
      MultiSwap
    | -- | <https://htmx.org/extensions/path-deps/> an extension for expressing path-based dependencies similar to intercoolerjs
      PathDeps
    | -- | <https://htmx.org/extensions/preload/> preloads selected href and hx-get targets based on rules you control.
      Preload
    | -- | <https://htmx.org/extensions/remove-me/> allows you to remove an element after a given amount of time
      RemoveMe
    | -- | <https://htmx.org/extensions/response-targets/> allows to specify different target elements to be swapped when different HTTP response codes are received
      ResponseTargets
    | -- | <https://htmx.org/extensions/restored/> allows you to trigger events when the back button has been pressed
      Restored
    | -- | <https://htmx.org/extensions/server-sent-events/> uni-directional server push messaging via EventSource
      ServerSentEvents
    | -- | <https://htmx.org/extensions/web-sockets/> bi-directional connection to WebSocket servers
      WebSockets
    | -- | <https://htmx.org/extensions/path-params/> allows to use parameters for path variables instead of sending them in query or body
      PathParams
    deriving (Eq, Ord, Show)

instance Render HtmxExtension where
    render = \case
        AjaxHeader -> "ajax-header"
        AlpineMorph -> "alpine-morph"
        ClassTools -> "class-tools"
        ClientSideTemplates -> "client-side-templates"
        Debug -> "debug"
        EventHeader -> "event-header"
        HeadSupport -> "head-support"
        IncludeVals -> "include-vals"
        JsonEnc -> "json-enc"
        Idiomorph -> "idiomorph"
        LoadingStates -> "loading-states"
        MethodOverride -> "method-override"
        MorphdomSwap -> "morphdom-swap"
        MultiSwap -> "multi-swap"
        PathDeps -> "path-deps"
        Preload -> "preload"
        RemoveMe -> "remove-me"
        ResponseTargets -> "response-targets"
        Restored -> "restored"
        ServerSentEvents -> "sse"
        WebSockets -> "ws"
        PathParams -> "path-params"
