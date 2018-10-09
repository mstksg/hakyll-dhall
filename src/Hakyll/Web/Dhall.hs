{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
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
-- There are three major workflows:
--
--     1. 'dExprCompiler', 'loadDhall', and 'dhallCompiler', for loading
--     underlying Dhall files, saving them into the Hakyll cache and later
--     interpreting them as values.
--
--     2. 'parseDhall' and 'parseDhallExpr', for parsing Dhall expressions
--     provided as strings, and resolving them while tracking dependencies.
--
--     3. 'dhallPrettyCompiler', for processing and re-formatting Dhall
--     files and presenting them as-is as a "final end-point".

module Hakyll.Web.Dhall (
  -- * Configuration and Options
    DhallCompilerOptions(..), DhallCompilerTrust(..)
  , defaultDhallCompilerOptions, dcoResolver, dcoMinimize, dcoNormalize
  -- ** Resolver Behaviors
  , DhallResolver(..), DefaultDhallResolver(..), drRemap, drFull
  -- * Import and Load Dhall Files
  -- ** As Dhall expressions
  , DExpr(..)
  , dExprCompiler, dExprCompilerWith
  -- *** From Hakyll cache
  , loadDhall, loadDhallSnapshot
  -- ** As Haskell types
  , dhallCompiler, dhallCompilerWith
  -- * Parse Dhall
  -- ** As Haskell types
  , parseDhall, parseDhallWith
  -- ** As Dhall Expressions
  , parseDhallExpr, parseDhallExprWith
  -- * Compile (prettify, normalize, re-map) Dhall text files
  , dhallPrettyCompiler
  , dhallRawPrettyCompiler, dhallFullPrettyCompiler
  , dhallPrettyCompilerWith
  -- * Internal Utilities
  , parseRawDhallExprWith
  , resolveDhallImports
  ) where

import           Control.Monad
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
import           Lens.Family                           (LensLike, LensLike', (.~), (&))
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
                     DRRaw  _ -> id
                     DRFull _ -> absurd
    get = do
        bs     <- Bi.getRemainingLazyByteString
        (_, t) <- either (fail . show) pure $
                    CBOR.deserialiseFromBytes CBOR.decodeTerm bs
        e      <- either (fail . show) pure $
                    decode t
        DExpr <$> traverse fromImport e
      where
        fromImport i = case defaultDhallResolver @a of
          DRRaw  _ -> pure i
          DRFull _ -> fail $
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

-- | Options for loading Dhall files.
data DhallCompilerOptions a = DCO
    { _dcoResolver :: DhallResolver a
      -- ^ Method to resolve imports encountered in files.  See
      -- documentation of 'DhallResolver' for more details.
    , _dcoMinimize :: Bool
      -- ^ Strictly for usage with 'dhallPrettyCompiler' and family: should
      -- the result be "minimized" (all in one line) or pretty-printed for
      -- human readability?
      --
      -- Can be useful for saving bandwidth.
      --
      -- Default: 'False'
    , _dcoNormalize :: Bool
      -- ^ If 'True', reduce expressions to normal form before using
      -- them.  Otherwise, attempts to do no normalization and presents
      -- the file as-is (stripping out comments and annotations)
      --
      -- Default: 'True'
    }
  deriving (Generic, Typeable)

-- | Lens for '_dcoResolver' field of 'DhallCompilerOptions'.
dcoResolver
    :: Functor f
    => LensLike f (DhallCompilerOptions a) (DhallCompilerOptions b) (DhallResolver a) (DhallResolver b)
dcoResolver f (DCO r m n) = (\r' -> DCO r' m n) <$> f r

-- | Lens for '_dcoMinimize' field of 'DhallCompilerOptions'.
dcoMinimize
    :: Functor f
    => LensLike' f (DhallCompilerOptions a) Bool
dcoMinimize f (DCO r m n) = (\m' -> DCO r m' n) <$> f m

-- | Lens for '_dcoNormalize' field of 'DhallCompilerOptions'.
dcoNormalize
    :: Functor f
    => LensLike' f (DhallCompilerOptions a) Bool
dcoNormalize f (DCO r m n) = DCO r m <$> f n

-- | Method for resolving imports.
--
-- The choice will determine the type of expression that 'loadDhallExpr'
-- and family will produce.
--
-- Note that at this moment, the only available options are "all or
-- nothing" --- either resolve all types imports completely and fully, or
-- none of them. Hopefully one day this library will offer the ability to
-- resolve only certain types of imports (environment variables, absolute
-- paths) and not others (remote network, local paths).
data DhallResolver :: K.Type -> K.Type where
    -- | Leave imports as imports, but optionally remap the destinations.
    DRRaw  :: { _drRemap :: Import -> Compiler (Expr Src Import)
                -- ^ Optionally remap the destinations.
                --
                -- __Important:__ '_drRemap' is /not/ applied recursively;
                -- it is only applied once.  Any imports in the resulting
                -- 'Expr Src Import' are not re-expanded.
                --
                -- Default: leave imports unchanged
              } -> DhallResolver Import
    -- | Completely resolve all imports in IO.  All imports within Hakyll
    -- project are tracked, and changes to dependencies will trigger
    -- rebuilds upstream.
    DRFull :: { _drTrust :: S.Set DhallCompilerTrust
                -- ^ Set of "trusted" import behaviors.  Files with
                -- external references or imports that aren't described in
                -- this set are always rebuilt every time.
                --
                -- Default: @'S.singleton' 'DCTRemote'@
                --
                -- That is, do not trust any dependencies on the local disk
                -- outside of the project directory, but trust that any URL
                -- imports remain unchanged.
              } -> DhallResolver X

-- | Lens for '_drRemap' field of 'DhallResolver'.
drRemap
    :: Functor f
    => LensLike' f (DhallResolver Import) (Import -> Compiler (Expr Src Import))
drRemap f (DRRaw r) = DRRaw <$> f r

-- | Lens for '_drFull' field of 'DhallResolver'.
drFull
    :: Functor f
    => LensLike' f (DhallResolver X) (S.Set DhallCompilerTrust)
drFull f (DRFull t) = DRFull <$> f t

-- | Default 'DhallCompilerOptions'.  If the type variable is not
-- inferrable, it can be helpful to use /TypeApplications/ syntax:
--
-- @
-- 'defaultCompilerOptions' \@'Import'         -- do not resolve imports
-- 'defaultCompilerOptions' \@'X'              -- resolve imports
-- @
defaultDhallCompilerOptions
    :: DefaultDhallResolver a
    => DhallCompilerOptions a
defaultDhallCompilerOptions = DCO
    { _dcoResolver  = defaultDhallResolver
    , _dcoMinimize  = False
    , _dcoNormalize = True
    }

-- | Helper typeclass to allow functions to be polymorphic over different
-- 'DhallResolver' types.
--
-- Provides default behavior for each resolver type.
class DefaultDhallResolver a where
    defaultDhallResolver :: DhallResolver a

-- | Leave all imports unchanged
instance DefaultDhallResolver Import where
    defaultDhallResolver = DRRaw $ pure . Embed

-- | Only trust remote imports remain unchanged.  Rebuild every time if any
-- absolute, home-directory-based, or environment variable imports are in
-- file.
instance DefaultDhallResolver X where
    defaultDhallResolver = DRFull $ S.singleton DCTRemote

-- | @'def' = 'defaultDhallCompilerOptions'@
instance DefaultDhallResolver a => Default (DhallCompilerOptions a) where
    def = defaultDhallCompilerOptions

-- TODO: other resolver functions
-- TODO: maybe one day hakyll can track environment variables?

-- | Essentially a Dhall pretty-printer, (optional) normalizer, and
-- re-formatter.  Compile the Dhall file as text according to default
-- 'DhallCompilerOptions'.  Note that this is polymorphic over both "raw"
-- and "fully resolved" versions; it must be called with
-- /TypeApplications/.
--
-- @
-- 'dhallRawPrettyCompiler'  = 'dhallPrettyCompiler' \@'Import'
-- 'dhallFullPrettyCompiler' = 'dhallPrettyCompiler' \@'X'
-- @
--
-- It might be more convenient to just use 'dhallRawCompiler' or
-- 'dhallFullCompiler'.
dhallPrettyCompiler
    :: forall a. (DefaultDhallResolver a, PP.Pretty a)
    => Compiler (Item String)
dhallPrettyCompiler = dhallPrettyCompilerWith @a defaultDhallCompilerOptions

-- TODO: way to only resolve Env and Absolute and Home?
-- Need to somehow hook into 'loadWith' so it can be recursive

-- | Compile the Dhall file as text according to default
-- 'DhallCompilerOptions' while leaving all imports unchanged and
-- unresolved.  Essentially a Dhall pretty-printer, (optional) normalizer,
-- and re-formatter.
dhallRawPrettyCompiler :: Compiler (Item String)
dhallRawPrettyCompiler = dhallPrettyCompilerWith @Import defaultDhallCompilerOptions

-- | Compile the Dhall file as text according to default
-- 'DhallCompilerOptions', resolving all imports in IO and tracking
-- dependencies.  Essentially a Dhall pretty-printer, (optional)
-- normalizer, and re-formatter.
dhallFullPrettyCompiler :: Compiler (Item String)
dhallFullPrettyCompiler = dhallPrettyCompilerWith @X defaultDhallCompilerOptions

-- | 'dhallPrettyCompiler', but with custom 'DhallCompilerOptions'.
dhallPrettyCompilerWith
    :: PP.Pretty a
    => DhallCompilerOptions a
    -> Compiler (Item String)
dhallPrettyCompilerWith dco = do
    DExpr e <- itemBody <$> dExprCompilerWith dco
    makeItem $ T.unpack (disp e)
  where
    disp
      | _dcoMinimize dco = pretty
      | otherwise        = PP.renderStrict
                         . PP.layoutSmart layoutOpts
                         . PP.unAnnotate
                         . prettyExpr

-- | Compile the underlying text file as a Dhall expression, wrapped in
-- a 'DExpr' newtype.  Mostly useful for pre-cacheing fully resolved Dhall
-- expressions into the Hakyll cache, which you can later interpret and
-- load  with 'loadDhall' or 'loadDhallSnapshot'.  A @'DExpr' a@ is an
-- @'Expr' 'Src' a@, but wrapped so that it has a 'Bi.Binary' instance that
-- is usable by the Hakyll cache.
--
-- For example, here is a rule to parse and cache all configuration files:
--
-- @
-- 'match' "config/**.dhall" $ do
--     'route' 'mempty'
--     'compile' $ 'dExprCompiler' \@'X'
-- @
--
-- This will save all of the dhall files in the directory ./config in the
-- Hakyll cache.  They can later be loaded and interpreted in the
-- 'Compiler' monad using:
--
-- @
-- 'loadDhall' 'auto' "config/my_config.dhall"
-- @
--
-- Note that if the @a@ is not inferrable by type inference (like in the
-- situation above), you can specify the @a@ using type application syntax
-- (like above).
--
-- Note that this is mostly useful for routes that match many different
-- files which will be interpreted as values of different types, or for
-- caching a single expression that you might want to interpret as
-- different types later.  If you want to parse and immediately interpret,
-- see 'dhallCompiler'.
--
-- Note also that this isn't really meant to be a "final end-point", but if
-- it is used as such, a pretty-printed version will be rendered to the
-- output directory, based on the 'Writable' instance of 'DExpr'.
dExprCompiler :: DefaultDhallResolver a => Compiler (Item (DExpr a))
dExprCompiler = dExprCompilerWith defaultDhallCompilerOptions

-- | 'dExprCompiler', but with custom 'DhallCompilerOptions'.
dExprCompilerWith
    :: DhallCompilerOptions a
    -> Compiler (Item (DExpr a))
dExprCompilerWith dco = do
    b <- itemBody <$> getResourceBody
    d <- takeDirectory . toFilePath <$> getUnderlying
    makeItem . DExpr =<< parseDhallExprWith dco (Just d) (T.pack b)

-- | Parse the underlying text file as a Dhall expression and directly
-- interpret it as a value of the given type.  Tracks all dependencies, so
-- will trigger rebuilds based on downstream changes.
dhallCompiler
    :: Type a
    -> Compiler (Item a)
dhallCompiler = dhallCompilerWith defaultDhallCompilerOptions

-- | 'dhallCompiler', but with custom 'DhallCompilerOptions'.
dhallCompilerWith
    :: DhallCompilerOptions X
    -> Type a
    -> Compiler (Item a)
dhallCompilerWith dco t = do
    DExpr e <- itemBody <$> dExprCompilerWith dco
    makeItem =<< interpretDhallCompiler t e

-- | Wrapper over 'load' and 'interpretDhallCompiler'.  Pulls up a 'DExpr'
-- compiled or saved into the Hakyll cache and interprets it as a value.
--
-- Expects item at identifier to be saved as @'DExpr' 'X'@ (possibly using
-- @'dExprCompiler' \@'X'@)
loadDhall
    :: Type a
    -> Identifier
    -> Compiler (Item a)
loadDhall t i = do
    DExpr e <- loadBody i
    makeItem =<< interpretDhallCompiler t e

-- | Wrapper over 'loadSnapshot' and 'interpretDhallCompiler'.  Pulls up
-- a 'DExpr' saved into the Hakyll cache as a snapshot and interprets it as
-- a value.
--
-- Expects item at identifier to be saved as @'DExpr' 'X'@ (possibly using
-- @'dExprCompiler' \@'X'@)
loadDhallSnapshot
    :: Type a
    -> Identifier
    -> Snapshot
    -> Compiler (Item a)
loadDhallSnapshot t i s = do
    DExpr e <- loadSnapshotBody i s
    makeItem =<< interpretDhallCompiler t e

-- | Parse a Dhall source.  Meant to be useful for patterns similar to
-- @dhall-to-text@.  If using examples from
-- <https://github.com/dhall-lang/dhall-text>, you can use:
--
-- @
-- 'parseDhallExpr' 'Nothing' ".\/make-items .\/people"
-- @
--
-- Any local dependencies within the project directory (./make-items and
-- ./people above, for example) are tracked by Hakyll, and so modifications
-- to required files will also cause upstream files to be rebuilt.
--
-- To directly obtain a Dhall expression, see 'parseDhallExpr'.
parseDhall
    :: Type a
    -> Maybe FilePath                   -- ^ Override directory root
    -> T.Text
    -> Compiler (Item a)
parseDhall = parseDhallWith defaultDhallCompilerOptions

-- | Version of 'parseDhall' taking custom 'DhallCompilerOptions'.
parseDhallWith
    :: DhallCompilerOptions X
    -> Type a
    -> Maybe FilePath                   -- ^ Override directory root
    -> T.Text
    -> Compiler (Item a)
parseDhallWith dco t fp b = do
    e <- parseDhallExprWith dco fp b
    makeItem =<< interpretDhallCompiler t e

-- | Interpret a fully resolved Dhall expression as a value of a type,
-- given a 'Type'. Run in 'Compiler' to integrate error handling with
-- Hakyll.
interpretDhallCompiler
    :: Type a
    -> Expr Src X
    -> Compiler a
interpretDhallCompiler t e = case rawInput t e of
    Nothing -> throwError . (terr:) . (:[]) $ case typeOf e of
      Left err  -> show err
      Right t0  -> T.unpack
                 . PP.renderStrict
                 . PP.layoutSmart layoutOpts
                 . diffNormalized (expected t)
                 $ t0
    Just x  -> pure x
  where
    terr = "Error interpreting Dhall expression as desired type."

-- | Version of 'parseDhall' that directly returns a Dhall expression,
-- instead of trying to interpret it into a custom Haskell type.
--
-- Any local dependencies within the project directory (./make-items and
-- ./people above, for example) are tracked by Hakyll, and so modifications
-- to required files will also cause upstream files to be rebuilt.
parseDhallExpr
    :: DefaultDhallResolver a
    => Maybe FilePath                   -- ^ Override directory root
    -> T.Text
    -> Compiler (Expr Src a)
parseDhallExpr = parseDhallExprWith defaultDhallCompilerOptions

-- | Version of 'parseDhallExpr' taking custom 'DhallCompilerOptions'.
parseDhallExprWith
    :: DhallCompilerOptions a
    -> Maybe FilePath                   -- ^ Override directory root
    -> T.Text
    -> Compiler (Expr Src a)
parseDhallExprWith dco d b = case _dcoResolver dco of
    DRRaw  _ -> norm <$> parseRawDhallExprWith dco b
    DRFull _ -> fmap norm
              . resolveDhallImports dco d
            =<< parseRawDhallExprWith (dco { _dcoResolver = defaultDhallResolver })
                  b
  where
    norm :: Eq b => Expr s b -> Expr s b
    norm
      | _dcoNormalize dco = normalize
      | otherwise         = id

-- | Version of 'parseDhallExprWith' that only acceps the 'DRRaw' resolver,
-- remapping the imports with the function in the 'DRRaw'.  Does not
-- perform any normalization.
parseRawDhallExprWith
    :: DhallCompilerOptions Import
    -> T.Text
    -> Compiler (Expr Src Import)
parseRawDhallExprWith DCO{..} b =
    case exprFromText "Hakyll.Web.Dhall.parseRawDhallExprWith" b of
      Left  e -> throwError . (:[]) $
        "Error parsing raw dhall file: " ++ show e
      Right e -> join <$> traverse (_drRemap _dcoResolver) e

-- | Resolve all imports in a parsed Dhall expression.
--
-- This implements the "magic" of dependency tracking: implemented so that
-- any local dependencies within the project directory are tracked by
-- Hakyll, and so modifications to required files will also cause upstream
-- files to be rebuilt.
resolveDhallImports
    :: DhallCompilerOptions X
    -> Maybe FilePath                   -- ^ Override directory root
    -> Expr Src Import
    -> Compiler (Expr Src X)
resolveDhallImports DCO{..} d e = do
    (res, imps) <- unsafeCompiler $ do
      iRef <- newIORef []
      res <- evalStateT (loadWith e) $
        emptyStatus (fromMaybe "./" d)
          & resolver .~ \i -> do
              liftIO $ modifyIORef iRef (i:)
              exprFromImport i
      (res,) <$> readIORef iRef
    compilerTellDependencies $ mapMaybe mkDep imps
    pure res
  where
    DRFull{..} = _dcoResolver
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
        | DCTLocal  `S.member` _drTrust -> Nothing
        | otherwise                     -> Just neverTrust
      Remote _
        | DCTRemote `S.member` _drTrust -> Nothing
        | otherwise                     -> Just neverTrust
      Env _
        | DCTEnv    `S.member` _drTrust -> Nothing
        | otherwise                     -> Just neverTrust
      Missing                           -> Just neverTrust
    neverTrust = PatternDependency mempty mempty
