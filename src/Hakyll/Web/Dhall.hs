{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}

module Hakyll.Web.Dhall where

-- module Hakyll.Web.Dhall (
--   ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.IORef
import           Data.Maybe
import           Data.Traversable
import           Data.Typeable                         (Typeable)
import           Dhall.Binary
import           Dhall.Core
import           Dhall.Import
import           Dhall.Parser
import           Dhall.Pretty
import           Dhall.TypeCheck
import           GHC.Generics                          (Generic)
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Writable
import           Lens.Micro
import           System.FilePath
import           System.IO
import qualified Codec.CBOR.Read                       as CBOR
import qualified Codec.CBOR.Term                       as CBOR
import qualified Codec.CBOR.Write                      as CBOR
import qualified Data.Binary                           as Bi
import qualified Data.Binary.Get                       as Bi
import qualified Data.Binary.Put                       as Bi
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import qualified Data.Text.Prettyprint.Doc             as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

newtype E = E { getE :: Expr Src X }
    deriving (Generic, Typeable)

instance Bi.Binary E where
    put = Bi.putBuilder
        . CBOR.toBuilder
        . CBOR.encodeTerm
        . encode V_1_0
        . fmap absurd
        . getE
    get = do
      bs <- Bi.getRemainingLazyByteString
      case CBOR.deserialiseFromBytes CBOR.decodeTerm bs of
        Left  e      -> fail $ show e
        Right (_, t) -> case decode t of
          Left  e -> fail $ show e
          Right e -> fmap E . for e $ \i -> fail $
               "Cannot deserialize dhall expression with imports: "
            ++ T.unpack (PP.renderStrict (PP.layoutSmart layoutOpts (PP.pretty i)))

instance Writable E where
    write fp e = withFile fp WriteMode $ \h ->
      PP.renderIO h
        . PP.layoutSmart layoutOpts
        . PP.unAnnotate
        . prettyExpr
        . getE
        . itemBody
        $ e

mkImport :: FilePath -> Import
mkImport fp = Import
    { importHashed = ImportHashed
        { hash       = Nothing
        , importType = Local Here f
        }
    , importMode = Code
    }
  where
    f = case T.pack <$> reverse (splitDirectories fp) of
          []   -> File (Directory []) ""
          x:xs -> File (Directory xs) x

data DhallCompilerTrust = DCTLocal
                        | DCTRemote
                        | DCTEnv
  deriving (Generic, Typeable, Show, Eq, Ord)

data DhallCompilerOptions = DCO
    { dcoTrust :: S.Set DhallCompilerTrust
    }

defaultDhallCompilerOptions :: DhallCompilerOptions
defaultDhallCompilerOptions = DCO
    { dcoTrust = S.empty
    }

dhallCompiler :: DhallCompilerOptions -> Compiler (Item E)
dhallCompiler DCO{..} = do
    fp <- toFilePath <$> getUnderlying
    let imp = mkImport fp
    (res, imps) <- unsafeCompiler $ do
      iRef <- newIORef []
      res <- evalStateT (loadWith (Embed imp)) $
        emptyStatus "./"
          & resolver .~ \i -> do
              liftIO $ modifyIORef iRef (i:)
              exprFromImport i
      (res,) <$> readIORef iRef
    compilerTellDependencies $ mapMaybe mkDep imps
    makeItem $ E res
  where
    mkDep :: Import -> Maybe Dependency
    mkDep i = case importType (importHashed i) of
      Local Here (File (Directory xs) x) -> Just
                                          . IdentifierDependency
                                          . fromFilePath
                                          . joinPath
                                          . map T.unpack
                                          . reverse
                                          $ x : xs
      Local _    _
        | DCTLocal  `S.member` dcoTrust -> Nothing
        | otherwise                     -> Just neverTrust
      Remote _
        | DCTRemote `S.member` dcoTrust -> Nothing
        | otherwise                     -> Just neverTrust
      Env _
        | DCTEnv    `S.member` dcoTrust -> Nothing
        | otherwise                     -> Just neverTrust
      Missing                           -> Just neverTrust
    neverTrust = PatternDependency mempty mempty
