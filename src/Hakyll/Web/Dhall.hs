{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- |
-- Module      : Hakyll.Web.Dhall
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Hakyll compiler and loader for Dhall files.  Functions are intended to
-- track all local dependencies within the project directory, so rebuilds
-- are properly triggered on up-stream imports.  Provides options for
-- customizing rebuilding behavior for network, environment variable, and
-- non-project local files.
--


module Hakyll.Web.Dhall (
  -- * Configuration and Options
    DhallCompilerOptions(..), DhallCompilerTrust(..)
  , defaultDhallCompilerOptions
  -- ** Resolver Behaviors
  , DhallResolver(..), DefaultDhallResolver(..)
  -- * Load Dhall Files
  -- ** As as custom Haskell types
  , loadDhall, loadDhallWith
  -- ** As raw expressions
  , loadDhallExpr, loadDhallExprWith
  -- * Compile Dhall Files
  , dhallCompiler
  , dhallRawCompiler, dhallFullCompiler
  , dhallCompilerWith
  -- * Internal Utilities
  , parseDhallWith
  , parseRawDhallWith
  , resolveDhallImports
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Data.Default.Class
import           Data.IORef
import           Data.Maybe
import           Data.Typeable                         (Typeable)
import           Dhall
import           Dhall.Binary
import           Dhall.Core
import           Dhall.Diff
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
import qualified Data.Kind                             as K
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import qualified Data.Text.Prettyprint.Doc             as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

-- | Newtype wrapper over @'Expr' 'Src' a@ (A Dhall expression) with an
-- appropriate 'Bi.Binary' instance, meant to be usable as a compilable
-- Hakyll result that can be saved with 'saveSnapshot', 'load', etc.
newtype DExpr a = DExpr { getDExpr :: Expr Src a }
    deriving (Generic, Typeable)

instance (DefaultDhallResolver a, PP.Pretty a) => Bi.Binary (DExpr a) where
    put = Bi.putBuilder
        . CBOR.toBuilder
        . CBOR.encodeTerm
        . encode V_1_0
        . fmap toImport
        . getDExpr
      where
        toImport = case defaultDhallResolver @a of
                     DRRaw _ -> id
                     DRFull  -> absurd
    get = do
        bs     <- Bi.getRemainingLazyByteString
        (_, t) <- either (fail . show) pure $
                    CBOR.deserialiseFromBytes CBOR.decodeTerm bs
        e      <- either (fail . show) pure $
                    decode t
        DExpr <$> traverse fromImport e
      where
        fromImport i = case defaultDhallResolver @a of
          DRRaw _ -> pure i
          DRFull  -> fail $
            "Unexpected import in deserialization of `DExpr X`: "
              ++ T.unpack (iStr i)
        iStr = PP.renderStrict
             . PP.layoutSmart layoutOpts
             . PP.pretty @Import

-- | Automatically "pretty prints" in multi-line form
instance PP.Pretty a => Writable (DExpr a) where
    write fp e = withFile fp WriteMode $ \h ->
      PP.renderIO h
        . PP.layoutSmart layoutOpts
        . PP.unAnnotate
        . prettyExpr
        . getDExpr
        . itemBody
        $ e

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
data DhallCompilerOptions a = DCO
    { dcoResolver :: DhallResolver a
        -- ^ Method to resolve imports encountered in files.  See
        -- documentation of 'DhallResolver' for more details.
    , dcoTrust :: S.Set DhallCompilerTrust
        -- ^ Set of "trusted" import behaviors.  Files with external
        -- references or imports that aren't described in this set are
        -- always rebuilt every time.
        --
        -- Default: @'S.singleton' 'DCTRemote'@
        --
        -- That is, do not trust any dependencies on the local disk outside
        -- of the project directory, but trust that any URL imports remain
        -- unchanged.
    , dcoMinimize :: Bool
        -- ^ Strictly for usage with 'dhallCompiler' and family: should the
        -- result be "minimized" (all in one line) or pretty-printed for
        -- human readability?
        --
        -- Can be useful for saving bandwidth.
        --
        -- Default: 'False'
    , dcoNormalize :: Bool
        -- ^ If 'True', reduce expressions to normal form before using
        -- them.
        --
        -- Default: 'True'
    }
  deriving (Generic, Typeable)

-- | Method for resolving imports.
--
-- The choice will determine the type of expression that 'loadDhallExpr'
-- and family will produce.
data DhallResolver :: K.Type -> K.Type where
    -- | Leave imports as imports, but optionally remap the destinations.
    --
    -- Default: leave imports unchanged
    DRRaw  :: { drRemap :: Import -> Compiler Import
              } -> DhallResolver Import
    -- | Completely resolve all imports in IO
    DRFull :: DhallResolver X

-- | Default 'DhallCompilerOptions'.  If the type variable is not
-- inferrable, it can be helpful to use /TypeApplications/ syntax:
--
-- @
-- 'defaultCompilerOptions' \@Import         -- do not resolve imports
-- 'defaultCompilerOptions' \@X              -- resolve imports
-- @
defaultDhallCompilerOptions
    :: DefaultDhallResolver a
    => DhallCompilerOptions a
defaultDhallCompilerOptions = DCO
    { dcoResolver  = defaultDhallResolver
    , dcoTrust     = S.singleton DCTRemote
    , dcoMinimize  = False
    , dcoNormalize = True
    }

-- | Helper typeclass to allow functions to be polymorphic over different
-- 'DhallResolver' types.
--
-- Provides default behavior for each resolver type.
class DefaultDhallResolver a where
    defaultDhallResolver :: DhallResolver a

-- | Leave all imports unchanged
instance DefaultDhallResolver Import where
    defaultDhallResolver = DRRaw pure

instance DefaultDhallResolver X where
    defaultDhallResolver = DRFull

-- | @'def' = 'defaultDhallCompilerOptions'@
instance DefaultDhallResolver a => Default (DhallCompilerOptions a) where
    def = defaultDhallCompilerOptions

-- | Compile the Dhall file as text according to default
-- 'DhallCompilerOptions'.  Note that this is polymorphic over both "raw"
-- and "fully resolved" versions; it must be called with /TypeApplications/
--
-- @
-- 'dhallRawCompiler'  = 'dhallCompiler' @'Import'
-- 'dhallFullCompiler' = 'dhallCompiler' @'X'
-- @
--
-- It might be more convenient to just use 'dhallRawCompiler' or
-- 'dhallFullCompiler'.
dhallCompiler
    :: forall a. (DefaultDhallResolver a, PP.Pretty a)
    => Compiler (Item String)
dhallCompiler = dhallCompilerWith @a defaultDhallCompilerOptions

-- TODO: way to only resolve Env and Absolute and Home?

-- | Compile the Dhall file as text according to default
-- 'DhallCompilerOptions' while leaving all imports unchanged and
-- unresolved.
dhallRawCompiler :: Compiler (Item String)
dhallRawCompiler = dhallCompilerWith @Import defaultDhallCompilerOptions

-- | Compile the Dhall file as text according to default
-- 'DhallCompilerOptions', resolving all imports in IO.
dhallFullCompiler :: Compiler (Item String)
dhallFullCompiler = dhallCompilerWith @X defaultDhallCompilerOptions

-- | 'dhallCompiler', but with custom 'DhallCompilerOptions'.
dhallCompilerWith
    :: PP.Pretty a
    => DhallCompilerOptions a
    -> Compiler (Item String)
dhallCompilerWith dco = do
    i <- getUnderlying
    b <- T.pack . itemBody <$> getResourceBody
    e <- itemBody <$> parseDhallWith dco i b
    makeItem $ T.unpack (disp e)
  where
    disp
      | dcoMinimize dco = pretty
      | otherwise       = PP.renderStrict
                        . PP.layoutSmart layoutOpts
                        . PP.unAnnotate
                        . prettyExpr

-- TODO: we need a way to re-label home, absolute, and environment variables

-- | Version of 'parseDhallWith' that only acceps the 'DRRaw' resolver,
-- remapping the imports with the function in the 'DRRaw'.
parseRawDhallWith
    :: DhallCompilerOptions Import
    -> Identifier
    -> T.Text
    -> Compiler (Item (Expr Src Import))
parseRawDhallWith DCO{..} i b =
    case exprFromText (toFilePath i) b of
      Left  e -> throwError . (:[]) $
        "Error parsing raw dhall file: " ++ show e
      Right e -> makeItem . normalize =<< traverse (drRemap dcoResolver) e

-- | Parse a Dhall source.
--
-- This encapsulates the "magic" of tracking dependencies.  Any local
-- dependencies within the project directory are tracked by Hakyll, and so
-- modifications to required files will also cause upstream files to be
-- rebuilt.
parseDhallWith
    :: DhallCompilerOptions a
    -> Identifier
    -> T.Text
    -> Compiler (Item (Expr Src a))
parseDhallWith dco i b = case dcoResolver dco of
    DRRaw _ -> fmap norm <$> parseRawDhallWith dco i b
    DRFull  -> (fmap . fmap) norm
             . traverse (resolveDhallImports dco i)
           =<< parseRawDhallWith (dco { dcoResolver = defaultDhallResolver })
                 i b
  where
    norm :: Eq b => Expr s b -> Expr s b
    norm
      | dcoNormalize dco = normalize
      | otherwise        = id

-- | Resolve all imports in a parsed Dhall expression.
--
-- Implemented so that any local dependencies within the project directory
-- are tracked by Hakyll, and so modifications to required files will also
-- cause upstream files to be rebuilt.
resolveDhallImports
    :: DhallCompilerOptions X
    -> Identifier
    -> Expr Src Import
    -> Compiler (Expr Src X)
resolveDhallImports DCO{..} ident e = do
    (res, imps) <- unsafeCompiler $ do
      iRef <- newIORef []
      res <- evalStateT (loadWith e) $
        emptyStatus (takeDirectory (toFilePath ident))
          & resolver .~ \i -> do
              liftIO $ modifyIORef iRef (i:)
              exprFromImport i
      (res,) <$> readIORef iRef
    compilerTellDependencies $ mapMaybe mkDep imps
    pure res
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

-- | Load and parse the body of the given 'Identifier' as a Dhall
-- expression.
--
-- If you wrap the result in 'DExpr', you can save the result as
-- a snapshot.
loadDhallExpr
    :: DefaultDhallResolver a
    => Identifier
    -> Compiler (Item (Expr Src a))
loadDhallExpr = loadDhallExprWith defaultDhallCompilerOptions

-- | Version of 'loadDhallExpr' taking custom 'DhallCompilerOptions'.
loadDhallExprWith
    :: DhallCompilerOptions a
    -> Identifier
    -> Compiler (Item (Expr Src a))
loadDhallExprWith dco i = do
    b <- T.pack <$> loadBody i
    parseDhallWith dco i b

-- | Load a value of type @a@ that is parsed from a Dhall file at the given
-- 'Identifier'.
loadDhall
    :: Type a
    -> Identifier
    -> Compiler (Item a)
loadDhall = loadDhallWith defaultDhallCompilerOptions

-- | Version of 'loadDhall' taking custom 'DhallCompilerOptions'.
loadDhallWith
    :: DhallCompilerOptions X
    -> Type a
    -> Identifier
    -> Compiler (Item a)
loadDhallWith dco t ident = traverse (inp t)
                        =<< loadDhallExprWith dco ident
  where
    inp :: Type a -> Expr Src X -> Compiler a
    inp t' e = case rawInput t' e of
      Nothing -> throwError . (terr:) . (:[]) $ case typeOf e of
        Left err  -> show err
        Right t0  -> T.unpack
                   . PP.renderStrict
                   . PP.layoutSmart layoutOpts
                   . diffNormalized (expected t)
                   $ t0
      Just x  -> pure x
    terr = "Error interpreting Dhall expression as desired type."
