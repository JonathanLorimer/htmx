{- |
Module      : Htmx.Lucid.Extension.IncludeVals
Description : Attribute for adding values to a request

This module defines an attribute that allows you to include additional values in a request
<https://github.com/bigskysoftware/htmx-extensions/blob/main/src/include-vals/README.md>
-}
module Htmx.Lucid.Extension.IncludeVals where

import Data.Text (Text)
import Lucid.Base (makeAttributes, Attributes)

-- | <https://github.com/bigskysoftware/htmx-extensions/blob/main/src/include-vals/README.md>
-- The value of this attribute is one or more name/value pairs, which will be evaluated as the fields in a javascript object literal.
-- i.e. "included:true, computed: computeValue()"
includeVals_ :: Text -> Attributes
includeVals_ = makeAttributes "include-vals"
