{- |
Module      : Lucid.Htmx.Head
Description : Utilities for including HTMX in the html head tag

This module defines utilities for installing HTMX and HTMX extensions
via the head tag in your html document
<https://htmx.org/docs/#installing>
-}
module Lucid.Htmx.Head where

import Data.Foldable (forM_)
import Data.Text (Text, pack)
import GHC.Natural (Natural)
import Lucid (Html, HtmlT, script_, src_)
import Lucid.Base (Attribute, makeAttribute)
import Lucid.Htmx.Extension
import Lucid.Htmx.Render

-- | Place in your @head_@ tag to use htmx attributes in your lucid template
useHtmx :: (Monad m) => HtmlT m ()
useHtmx = script_ [src_ htmxSrc] ("" :: Html ())

-- | Place in your template after @useHtmx@, but before where the extension is used via @hxExt_@
useHtmxExtension :: (Monad m) => Text -> HtmlT m ()
useHtmxExtension ext = script_ [src_ $ htmxSrc <> extensionPath ext] ("" :: Html ())

-- | A typesafe version of 'useHtmxExtension' based on the "included" extensions
-- that the htmx codebase is tested against
useHtmxExt :: (Monad m) => HtmxExtension -> HtmlT m ()
useHtmxExt ext = script_ [src_ $ htmxSrc <> extensionPath (render ext)] ("" :: Html ())

-- | A typesafe version of 'useHtmxExtension' based on the "included" extensions
-- that the htmx codebase is tested against
useHtmxExts :: (Monad m) => [HtmxExtension] -> HtmlT m ()
useHtmxExts exts = forM_ exts $ \ext ->
    script_ [src_ $ htmxSrc <> extensionPath (render ext)] ("" :: Html ())

-- | Choose the version of htmx to use using a 3-tuple representing semantic versioning
useHtmxVersion :: (Monad m) => (Natural, Natural, Natural) -> HtmlT m ()
useHtmxVersion semVer = script_ [src_ $ htmxSrcWithSemVer semVer] ("" :: Html ())

-- | Choose the version of a htmx extension you want to use.
-- Should only be used when using @useHtmxVersion@ and the semantic version should be the same
useHtmxVersionExtension ::
    (Monad m) => (Natural, Natural, Natural) -> Text -> HtmlT m ()
useHtmxVersionExtension semVer ext =
    script_ [src_ $ htmxSrcWithSemVer semVer <> extensionPath ext] ("" :: Html ())

-- | This is the recommended version of htmx for using this library
-- (lucid-htmx). It is the version of the documentation that the implementation
-- is based off of.
recommendedVersion :: (Natural, Natural, Natural)
recommendedVersion = (1, 9, 12)

htmxSrc :: Text
htmxSrc = "https://unpkg.com/htmx.org"

showT :: (Show a) => a -> Text
showT = pack . show

htmxSrcWithSemVer :: (Natural, Natural, Natural) -> Text
htmxSrcWithSemVer (major, minor, patch) =
    htmxSrc
        <> "@"
        <> showT major
        <> "."
        <> showT minor
        <> "."
        <> showT patch

extensionPath :: Text -> Text
extensionPath ext = "/dist/ext/" <> ext <> ".js"
