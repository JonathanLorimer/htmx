module Lucid.Htmx.Extension.IncludeVals where

import Data.Text (Text)
import Lucid
import Lucid.Base (makeAttribute)

-- | <https://github.com/bigskysoftware/htmx-extensions/blob/main/src/include-vals/README.md>
-- The value of this attribute is one or more name/value pairs, which will be evaluated as the fields in a javascript object literal.
-- i.e. "included:true, computed: computeValue()"
inlcudeVals_ ::  Text -> Attribute
inlcudeVals_ = makeAttribute "include-vals"
