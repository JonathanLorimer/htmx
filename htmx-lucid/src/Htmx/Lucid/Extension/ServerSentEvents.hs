{- |
Module      : Htmx.Lucid.Extension.ServerSentEvents
Description : Attribute for connecting to an sse stream

The Server Sent Events extension connects to an EventSource directly from HTML.
It manages the connections to your web server, listens for server events, and
then swaps their contents into your htmx webpage in real-time.
<https://github.com/bigskysoftware/htmx-extensions/blob/main/src/sse/README.md>
-}
module Htmx.Lucid.Extension.ServerSentEvents where

import Data.Text (Text)
import Lucid.Base (makeAttributes, Attributes)

-- | <https://github.com/bigskysoftware/htmx-extensions/tree/main/src/sse#connecting-to-an-sse-server>
-- Provide the url to connect to, in order to establish an SSE channel.
sseConnect_ :: Text -> Attributes
sseConnect_ = makeAttributes "sse-connect"

-- | A stronger type for the different kinds of events permitted by 'sseSwap_'
data SseEventKind = Named Text | UnNamed

-- | <https://github.com/bigskysoftware/htmx-extensions/tree/main/src/sse#receiving-named-events>
-- event name to listen for in an SSE message, the contents of the message will
-- be swapped into the tag this attribute is on. For named events the message structure is as follows:
-- 
-- @
-- event: EventName
-- data: <div>Content to swap into your HTML page.</div>sseSwap_ :: Text -> Attributes
-- @
--
-- For unnamed events the message structure should look like this:
--
-- @
-- data: <div>Content to swap into your HTML page.</div>sseSwap_ :: Text -> Attributes
-- @
sseSwap_ :: SseEventKind -> Attributes
sseSwap_ = \case  
  Named eventName -> makeAttributes "sse-swap" eventName
  UnNamed -> makeAttributes "sse-swap" "message"
