{-# LANGUAGE OverloadedStrings #-}

import           Dhall
import           Hakyll
import           Hakyll.Web.Dhall

main :: IO ()
main = hakyll $ do
    match "test-dhall/**" $ do
      route idRoute
      compile dhallFullPrettyCompiler

    create ["testparse.txt"] $ do
      route idRoute
      compile $ parseDhall
                  Nothing
                  (auto :: Decoder String)
                  "./test-dhall/parse-test.dhall"
