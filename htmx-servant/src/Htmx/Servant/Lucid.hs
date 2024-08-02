{- |
Module      : Htmx.Servant.Lucid
Description : Typesafe versions of HTMX request tags

This module exports Lucid combinators that leverage the Servant 'Link'
type to guarantee that they are live URLs, therefore making the requests
"safe".
-}
module Htmx.Servant.Lucid (
    hxDeleteSafe_,
    hxGetSafe_,
    hxPatchSafe_,
    hxPostSafe_,
    hxPushUrlSafe_,
    hxPutSafe_,
)
where

import Data.Text (Text)
import Htmx.Lucid.Core (
    hxGet_,
    hxPost_,
    hxPushUrl_,
 )
import Htmx.Lucid.Extra (
    hxDelete_,
    hxPatch_,
    hxPut_,
 )
import Lucid.Base (Attributes)
import Servant.API (ToHttpApiData (..), toUrlPiece)
import Servant.Links (Link)

hxDeleteSafe_ :: Link -> Attributes
hxDeleteSafe_ = hxDelete_ . toUrl

hxGetSafe_ :: Link -> Attributes
hxGetSafe_ = hxGet_ . toUrl

hxPatchSafe_ :: Link -> Attributes
hxPatchSafe_ = hxPatch_ . toUrl

hxPostSafe_ :: Link -> Attributes
hxPostSafe_ = hxPost_ . toUrl

hxPushUrlSafe_ :: Either Bool Link -> Attributes
hxPushUrlSafe_ boolOrUrl = hxPushUrl_ $ case boolOrUrl of
    Left bool -> if bool then "true" else "false"
    Right url -> toUrl url

hxPutSafe_ :: Link -> Attributes
hxPutSafe_ = hxPut_ . toUrl

toUrl :: (ToHttpApiData a) => a -> Text
toUrl = ("/" <>) . toUrlPiece
