{-# LANGUAGE OverloadedStrings #-}

import           Hakyll
import           Hakyll.Web.Dhall

main :: IO ()
main = hakyll $
    match "test-dhall/**" $ do
      route idRoute
      -- route $ gsubRoute "dhall" (const "txt")
      compile $ dhallCompiler defaultDhallCompilerOptions

