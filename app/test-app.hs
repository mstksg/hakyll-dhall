{-# LANGUAGE OverloadedStrings #-}

import           Dhall
import           Hakyll
import           Hakyll.Web.Dhall

main :: IO ()
main = hakyll $ do
    match "test-dhall/**" $ do
      route idRoute
      compile dhallFullCompiler

    create ["testparse.txt"] $ do
      route idRoute
      compile $ parseDhall (auto :: Type String) Nothing "./test-dhall/parse-test.dhall"
