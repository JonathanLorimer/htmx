{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
import Lucid
import Lucid.Htmx
import System.IO (stdout, hSetEncoding, utf8)
import Data.Text.Lazy.IO as L

main :: IO ()
main = do
  hSetEncoding stdout utf8
  L.hPutStr stdout (renderText template1)

template1 :: Html ()
template1 =
  div_ [id_ "someId" , hxPost_ "/some/url" ] "Content of div"
