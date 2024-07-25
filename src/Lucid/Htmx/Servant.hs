{- |
Module      : Lucid.Htmx.Servant
Description : Typesafe versions of HTMX request tags

This module exports Lucid combinators that leverage the Servant 'Link'
type to guarantee that they are live URLs, therefore making the requests
"safe".
-}
module Lucid.Htmx.Servant (
    hxDeleteSafe_,
    hxGetSafe_,
    hxPatchSafe_,
    hxPostSafe_,
    hxPushUrlSafe_,
    hxPutSafe_,
)
where

import Data.Text (Text)
import Lucid.Base (Attribute)
import Lucid.Htmx (
    hxDelete_,
    hxGet_,
    hxPatch_,
    hxPost_,
    hxPushUrl_,
    hxPut_,
 )
import Servant.API (ToHttpApiData (..), toUrlPiece)
import Servant.Links (Link)

hxDeleteSafe_ :: Link -> Attribute
hxDeleteSafe_ = hxDelete_ . toUrl

hxGetSafe_ :: Link -> Attribute
hxGetSafe_ = hxGet_ . toUrl

hxPatchSafe_ :: Link -> Attribute
hxPatchSafe_ = hxPatch_ . toUrl

hxPostSafe_ :: Link -> Attribute
hxPostSafe_ = hxPost_ . toUrl

hxPushUrlSafe_ :: Either Bool Link -> Attribute
hxPushUrlSafe_ boolOrUrl = hxPushUrl_ $ case boolOrUrl of
    Left bool -> if bool then "true" else "false"
    Right url -> toUrl url

hxPutSafe_ :: Link -> Attribute
hxPutSafe_ = hxPut_ . toUrl

toUrl :: (ToHttpApiData a) => a -> Text
toUrl = ("/" <>) . toUrlPiece
