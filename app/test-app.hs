{-# LANGUAGE OverloadedStrings #-}

import           Dhall
import           Hakyll
import           Hakyll.Web.Dhall

main :: IO ()
main = hakyll $ do
    match "test-dhall/**" $ do
      route idRoute
      compile dhallCompiler

    create ["testparse.txt"] $ do
      route idRoute
      compile $ loadDhall (auto :: Type String) "test-dhall/parse-test.dhall"
