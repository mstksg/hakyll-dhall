{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# OPTIONS_GHC -Wno-orphans    #-}

module Hakyll.Web.Dhall (
  -- * Configuration and Options
    DhallCompilerOptions(..), DhallCompilerTrust(..)
  , defaultDhallCompilerOptions
  -- * Load Dhall Files
  -- ** As as custom Haskell types
  , loadDhall, loadDhallWith
  -- ** As raw expressions
  , DExpr(..)
  , loadDExpr, loadDExprWith
  -- * Compile Dhall Files
  , dhallCompiler, dhallCompilerWith
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.Default.Class
import           Data.IORef
import           Data.Maybe
import           Data.Traversable
import           Data.Typeable                         (Typeable)
import           Dhall
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

-- | Newtype wrapper over @'Expr' 'Src' 'X'@ (A Dhall expression) with an
-- appropriate 'Bi.Binary' instance, meant to be usable as a compilable
-- Hakyll result
newtype DExpr = DExpr { getDExpr :: Expr Src X }
    deriving (Generic, Typeable)

instance Bi.Binary DExpr where
    put = Bi.putBuilder
        . CBOR.toBuilder
        . CBOR.encodeTerm
        . encode V_1_0
        . fmap absurd
        . getDExpr
    get = do
        bs     <- Bi.getRemainingLazyByteString
        (_, t) <- either (fail . show) pure $
                    CBOR.deserialiseFromBytes CBOR.decodeTerm bs
        e      <- either (fail . show) pure $
                    decode t
        fmap DExpr . for e $ \i -> fail $
          "Cannot deserialize dhall expression with imports: "
            ++ T.unpack (iStr i)
      where
        iStr = PP.renderStrict
             . PP.layoutSmart layoutOpts
             . PP.pretty

-- TODO: maybe offer pretty v unpretty?
instance Writable DExpr where
    write fp = write fp . fmap getDExpr

instance PP.Pretty a => Writable (Expr s a) where
    write fp e = withFile fp WriteMode $ \h ->
      PP.renderIO h
        . PP.layoutSmart layoutOpts
        . PP.unAnnotate
        . prettyExpr
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

-- | Types of external imports that a Dhall file may have.
data DhallCompilerTrust = DCTLocal
                            -- ^ File on local filesystem outside of
                            -- project directory, and therefore not tracked
                            -- by Hakyll
                        | DCTRemote
                            -- ^ Link to remote resource over a network
                            -- connection
                        | DCTEnv
                            -- ^ Reference to environment variable on
                            -- machine
  deriving (Generic, Typeable, Show, Eq, Ord)

-- | Options for loading Dhall files
data DhallCompilerOptions = DCO
    { dcoTrust :: S.Set DhallCompilerTrust
        -- ^ Set of "trusted" import behaviors.  Files with external
        -- references or imports that aren't described in this set are
        -- always rebuilt every time.
    }
  deriving (Generic, Typeable, Show, Eq, Ord)

-- | Default 'DhallCompilerOptions'.  Default behavior is to trust no
-- external imports, and to always rebuild files that contain any external
-- imports (that is, files outside of the project directory, references
-- to a remote network location, or references to environment variables).
defaultDhallCompilerOptions :: DhallCompilerOptions
defaultDhallCompilerOptions = DCO
    { dcoTrust = S.empty
    }

-- | @'def' = 'defaultDhallCompilerOptions'@
instance Default DhallCompilerOptions where
    def = defaultDhallCompilerOptions

-- TODO: this should leave network locations unnormalized?
--
-- TODO: Maybe this should return String instaed, with optiosn for
-- normalized vs unnormalized vs pretty vs not pretty
--
-- basically this could be used to make a dhall server?
dhallCompiler :: Compiler (Item DExpr)
dhallCompiler = dhallCompilerWith defaultDhallCompilerOptions

dhallCompilerWith
    :: DhallCompilerOptions
    -> Compiler (Item DExpr)
dhallCompilerWith dco = loadDExprWith dco =<< getUnderlying

loadDExpr
    :: Identifier
    -> Compiler (Item DExpr)
loadDExpr = loadDExprWith defaultDhallCompilerOptions

loadDExprWith
    :: DhallCompilerOptions
    -> Identifier
    -> Compiler (Item DExpr)
loadDExprWith DCO{..} ident = do
    (res, imps) <- unsafeCompiler $ do
      iRef <- newIORef []
      res <- evalStateT (loadWith (Embed (mkImport (toFilePath ident)))) $
        emptyStatus "./"
          & resolver .~ \i -> do
              liftIO $ modifyIORef iRef (i:)
              exprFromImport i
      (res,) <$> readIORef iRef
    compilerTellDependencies $ mapMaybe mkDep imps
    makeItem $ DExpr res
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

loadDhallWith
    :: DhallCompilerOptions
    -> Type a
    -> Identifier
    -> Compiler (Item a)
loadDhallWith dco t ident = traverse (inp t . getDExpr)
                        =<< loadDExprWith dco ident
  where
    inp :: Type a -> Expr Src X -> Compiler a
    inp t' e = case rawInput t' e of
      Nothing -> throwError ["Error interpreting Dhall expression as desired type."]
      Just x  -> pure x

loadDhall
    :: Type a
    -> Identifier
    -> Compiler (Item a)
loadDhall = loadDhallWith defaultDhallCompilerOptions
