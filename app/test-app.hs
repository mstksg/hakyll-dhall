{-# LANGUAGE OverloadedStrings #-}

import           Data.Default.Class
import           Dhall
import           Hakyll
import           Hakyll.Web.Dhall

main :: IO ()
main = hakyll $ do
    match "test-dhall/**" $ do
      route idRoute
      -- route $ gsubRoute "dhall" (const "txt")
      compile dhallCompiler

    create ["testparse.txt"] $ do
      route idRoute
      compile $ dhallTypeCompilerWith
                  (def { dcoPath = Just "test-dhall/parse-test.dhall" })
                  (auto :: Type String)
