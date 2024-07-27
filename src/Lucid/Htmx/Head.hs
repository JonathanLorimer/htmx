{- |
Module      : Lucid.Htmx.Head
Description : Utilities for including HTMX in the html head tag

This module defines utilities for installing HTMX and HTMX extensions
via the head tag in your html document
<https://htmx.org/docs/#installing>
-}
module Lucid.Htmx.Head (
    useHtmx,
    useHtmxVersion,
    useHtmxExtension,
    useHtmxExtensionV,
    useHtmxExtensions,
    useHtmxExtensionsV,
    recommendedVersion,
    htmxSrc,
    htmxSrcWithSemVer,
    htmxExtSrc
) where

import Data.Foldable (forM_)
import Data.Text (Text, pack)
import GHC.Natural (Natural)
import Lucid (Html, HtmlT, script_, src_)
import Lucid.Base (Attribute, makeAttribute)
import Lucid.Htmx.Extension
import Lucid.Htmx.Render


-- | Place in your template after @useHtmx@, but before where the extension is used via @hxExt_@
-- NOTE: This uses 'recommendedVersion' as the version section of the URL
useHtmxExtension :: (Monad m) => HtmxExtension -> HtmlT m ()
useHtmxExtension = useHtmxExtensionV recommendedVersion

-- | Same as 'useHtmxExt' but lets you choose the version url
useHtmxExtensionV :: (Monad m) => (Natural, Natural, Natural) -> HtmxExtension -> HtmlT m ()
useHtmxExtensionV v ext = script_ [src_ $ htmxExtSrc v (render ext)] ("" :: Html ())

-- | A typesafe version of 'useHtmxExtension' based on the "included" extensions
-- that the htmx codebase is tested against
-- NOTE: This uses 'recommendedVersion' as the version section of the URL
useHtmxExtensions :: (Monad m) => [HtmxExtension] -> HtmlT m ()
useHtmxExtensions exts = forM_ exts useHtmxExtension

-- | Same as 'useHtmxExts' but with a versioned url
useHtmxExtensionsV :: (Monad m) => (Natural, Natural, Natural) -> [HtmxExtension] -> HtmlT m ()
useHtmxExtensionsV v exts = forM_ exts (useHtmxExtensionV v)

-- | Place in your @head_@ tag to use htmx attributes in your lucid template
useHtmx :: (Monad m) => HtmlT m ()
useHtmx = useHtmxVersion recommendedVersion

-- | Choose the version of htmx to use using a 3-tuple representing semantic versioning
useHtmxVersion :: (Monad m) => (Natural, Natural, Natural) -> HtmlT m ()
useHtmxVersion semVer = script_ [src_ $ htmxSrcWithSemVer semVer] ("" :: Html ())

-- | This is the recommended version of htmx for using this library
-- (lucid-htmx). It is the version of the documentation that the implementation
-- is based off of.
recommendedVersion :: (Natural, Natural, Natural)
recommendedVersion = (2, 0, 1)

htmxSrc :: Text
htmxSrc = "https://unpkg.com/htmx.org"

showT :: (Show a) => a -> Text
showT = pack . show

showSemVer :: (Natural, Natural, Natural) -> Text
showSemVer (major, minor, patch) = "@"
    <> showT major
    <> "."
    <> showT minor
    <> "."
    <> showT patch

htmxSrcWithSemVer :: (Natural, Natural, Natural) -> Text
htmxSrcWithSemVer ver =
    htmxSrc <> showSemVer ver

htmxExtSrc :: (Natural, Natural, Natural) -> Text -> Text
htmxExtSrc ver ext = 
    "https://unpkg.com/htmx-ext-" 
    <> ext 
    <> showSemVer ver 
    <> "/"
    <> ext
    <> ".js"
