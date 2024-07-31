{- |
Module      : Htmx.Render
Description : Typeclass for rendering domain types as HTMX compatible 'Text'

This module defines a typeclass that doesn't have the historical baggage or
connotations of other text serialization typeclasses (like 'Show' or Display).
The semantics of this class are supposed to be HTMX specific, i.e. serializing
attribute values
-}
module Htmx.Render where

import Data.Text (Text)

-- | A typeclass for rendering domain types into attribute values
class Render a where
    render :: a -> Text
