{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
  -- * Import and Load Dhall Files
  -- ** As Dhall expressions
    DExpr(..)
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
  , renderDhallExprWith
  -- * Configuration and Options
  , DhallCompilerOptions(..), DhallCompilerTrust(..)
  , defaultDhallCompilerOptions, dcoResolver, dcoMinimize, dcoNormalize
  -- ** Resolver Behaviors
  , DhallResolver(..), DefaultDhallResolver(..), drRemap, drFull
  -- * Internal Utilities
  , interpretDhallCompiler
  , parseRawDhallExprWith
  , resolveDhallImports
  ) where

import           Control.Monad
import           Control.Monad.Error.Class
import           Data.Either.Validation
import           Control.Monad.Trans.State.Strict
import           Data.Default.Class
import           Data.Kind
import           Data.Maybe                            as M
import           Data.Typeable                         (Typeable)
import           Data.Void
import           Dhall hiding                          (map)
import           Dhall.Binary
import           Dhall.Core
import           Dhall.Diff
import           Dhall.Import
import           Dhall.Parser
import           Dhall.Pretty
import           Dhall.TypeCheck
import           Hakyll.Core.Compiler
import           Hakyll.Core.Compiler.Internal
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           Hakyll.Core.Item
import           Hakyll.Core.Writable
import           Lens.Micro
import           Lens.Micro.TH
import           System.FilePath
import           System.IO
import qualified Data.Binary                           as Bi
import qualified Data.Binary.Get                       as Bi
import qualified Data.Binary.Put                       as Bi
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import qualified Prettyprinter                         as PP
import qualified Prettyprinter.Render.Text             as PP
import qualified Dhall.Map                             as DM

-- | Newtype wrapper over @'Expr' 'Src' a@ (A Dhall expression) with an
-- appropriate 'Bi.Binary' instance, meant to be usable as a compilable
-- Hakyll result that can be saved with 'saveSnapshot', 'load', etc.
newtype DExpr a = DExpr { getDExpr :: Expr Src a }
    deriving (Generic, Typeable)

instance Bi.Binary (DExpr Void) where
    put = Bi.putLazyByteString
        . encodeExpression
        . denote
        . getDExpr
    get = do
        bs     <- Bi.getRemainingLazyByteString
        either (fail . show) (pure . DExpr . denote @Void) $ decodeExpression bs

-- | Automatically "pretty prints" in multi-line form.  For more
-- fine-grained results, see 'dhallPrettyCompilerWith' and family.
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
data DhallResolver :: Type -> Type where
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
              } -> DhallResolver Void

-- | Lens for '_drRemap' field of 'DhallResolver'.
drRemap
    :: Lens' (DhallResolver Import) (Import -> Compiler (Expr Src Import))
drRemap f (DRRaw r) = DRRaw <$> f r

-- | Lens for '_drFull' field of 'DhallResolver'.
drFull
    :: Lens' (DhallResolver Void) (S.Set DhallCompilerTrust)
drFull f (DRFull t) = DRFull <$> f t

makeLenses ''DhallCompilerOptions

-- | Default 'DhallCompilerOptions'.  If the type variable is not
-- inferrable, it can be helpful to use /TypeApplications/ syntax:
--
-- @
-- 'defaultDhallCompilerOptions' \@'Import'     -- do not resolve imports
-- 'defaultDhallCompilerOptions' \@'Void'          -- resolve imports
-- @
--
-- Default values are:
--
-- @
-- 'DCO'
--   { '_dcoResolver'  = 'defaultDhallResolver'
--   , '_dcoMinimize'  = 'False'
--   , '_dcoNormalize' = 'True'
--   }
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
instance DefaultDhallResolver Void where
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
-- 'dhallFullPrettyCompiler' = 'dhallPrettyCompiler' \@'Void'
-- @
--
-- It might be more convenient to just use 'dhallRawCompiler' or
-- 'dhallFullCompiler'.
dhallPrettyCompiler
    :: forall a. DefaultDhallResolver a
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
dhallFullPrettyCompiler = dhallPrettyCompilerWith @Void defaultDhallCompilerOptions

-- | 'dhallPrettyCompiler', but with custom 'DhallCompilerOptions'.
dhallPrettyCompilerWith
    :: DhallCompilerOptions a
    -> Compiler (Item String)
dhallPrettyCompilerWith dco = do
    DExpr e <- itemBody <$> dExprCompilerWith dco
    makeItem . T.unpack $ renderDhallExprWith dco e

-- | Format and pretty-print an 'Expr' according to options in a given
-- 'DhallCompilerOptions'.
renderDhallExprWith
    :: DhallCompilerOptions a
    -> Expr Src a
    -> T.Text
renderDhallExprWith DCO{..} = case _dcoResolver of
    DRRaw  _ -> go
    DRFull _ -> go
  where
    go :: (PP.Pretty a, Eq a) => Expr Src a -> T.Text
    go  | _dcoMinimize = pretty . norm
        | otherwise    = PP.renderStrict
                       . PP.layoutSmart layoutOpts
                       . PP.unAnnotate
                       . prettyExpr
                       . norm
     where
       norm
         | _dcoNormalize = normalize
         | otherwise     = id

-- | Compile the underlying text file as a Dhall expression, wrapped in
-- a 'DExpr' newtype.  Mostly useful for pre-cacheing fully resolved Dhall
-- expressions into the Hakyll cache, which you can later interpret and
-- load  with 'loadDhall' or 'loadDhallSnapshot'.  A @'DExpr' a@ is an
-- @'Expr' 'Src' a@, but wrapped so that it has a 'Bi.Binary' instance that
-- is usable by the Hakyll cache.  Tracks all dependencies, so will trigger
-- rebuilds of items that depend on it if any downstream dhall files are
-- modified.
--
-- For example, here is a rule to parse and cache all Dhall files in the
-- directory ./config:
--
-- @
-- 'match' "config/**.dhall" $ do
--     'route' 'mempty'
--     'compile' $ 'dExprCompiler' \@'Void'
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
-- This is mostly useful for routes that match many different
-- files which will be interpreted as values of different types, or for
-- caching a single expression that you might want to interpret as
-- different types later.  If you want to parse and immediately interpret,
-- see 'dhallCompiler'.
--
-- _Note:_ If the @a@ is not inferrable by type inference (like in the
-- situation above), you can specify the @a@ using type application syntax
-- (like above).
--
-- _Note:_ This isn't really meant to be a "final end-point", but if it is
-- used as such, a pretty-printed version will be rendered to the output
-- directory, based on the 'Writable' instance of 'DExpr'.
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
    :: Decoder a
    -> Compiler (Item a)
dhallCompiler = dhallCompilerWith defaultDhallCompilerOptions

-- | 'dhallCompiler', but with custom 'DhallCompilerOptions'.
dhallCompilerWith
    :: DhallCompilerOptions Void
    -> Decoder a
    -> Compiler (Item a)
dhallCompilerWith dco t = do
    DExpr e <- itemBody <$> dExprCompilerWith dco
    makeItem =<< interpretDhallCompiler t e

-- | Wrapper over 'load' and 'interpretDhallCompiler'.  Pulls up a 'DExpr'
-- compiled or saved into the Hakyll cache and interprets it as a value.
--
-- Expects item at identifier to be saved as @'DExpr' 'Void'@ (possibly using
-- @'dExprCompiler' \@'Void'@)
--
-- Tracks dependencies properly, so any pages or routes that use the saved
-- Dhall expression will re-build if any of the downstream Dhall files are
-- edited.
loadDhall
    :: Decoder a
    -> Identifier
    -> Compiler (Item a)
loadDhall t i = do
    DExpr e <- loadBody i
    makeItem =<< interpretDhallCompiler t e

-- | Wrapper over 'loadSnapshot' and 'interpretDhallCompiler'.  Pulls up
-- a 'DExpr' saved into the Hakyll cache as a snapshot and interprets it as
-- a value.
--
-- Expects item at identifier to be saved as @'DExpr' 'Void'@ (possibly using
-- @'dExprCompiler' \@'Void'@)
--
-- Tracks dependencies properly, so any pages or routes that use the saved
-- Dhall expression will re-build if any of the downstream Dhall files are
-- edited.
loadDhallSnapshot
    :: Decoder a
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
    :: Maybe FilePath                   -- ^ Override directory root
    -> Decoder a
    -> T.Text
    -> Compiler (Item a)
parseDhall = parseDhallWith defaultDhallCompilerOptions

-- | Version of 'parseDhall' taking custom 'DhallCompilerOptions'.
parseDhallWith
    :: DhallCompilerOptions Void
    -> Maybe FilePath                   -- ^ Override directory root
    -> Decoder a
    -> T.Text
    -> Compiler (Item a)
parseDhallWith dco fp t b = do
    e <- parseDhallExprWith dco fp b
    makeItem =<< interpretDhallCompiler t e

-- | Interpret a fully resolved Dhall expression as a value of a type,
-- given a 'Decoder'.  You can use this to integrate error handling with
-- Hakyll, since it is run in a 'Compiler'.
interpretDhallCompiler
    :: Decoder a
    -> Expr Src Void
    -> Compiler a
interpretDhallCompiler t e = case rawInput t e of
    Nothing -> throwError . (terr:) . (:[]) $ case typeOf e of
      Left err  -> show err
      Right t0  -> case expected t of
        Success tExpect -> T.unpack
                 . PP.renderStrict
                 . PP.layoutSmart layoutOpts
                 . doc
                 . diffNormalized tExpect
                 $ t0
        Failure q -> showDhallErrors "" q
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
      Left  e -> throwError
        [ "Error parsing raw dhall file"
        , "<<" ++ T.unpack b ++ ">>"
        , show e
        ]
      Right e -> join <$> traverse (_drRemap _dcoResolver) e

-- | Resolve all imports in a parsed Dhall expression.
--
-- This implements the "magic" of dependency tracking: implemented so that
-- any local dependencies within the project directory are tracked by
-- Hakyll, and so modifications to required files will also cause upstream
-- files to be rebuilt.
resolveDhallImports
    :: DhallCompilerOptions Void
    -> Maybe FilePath                   -- ^ Override directory root
    -> Expr Src Import
    -> Compiler (Expr Src Void)
resolveDhallImports DCO{..} d e = do
    (res, Status{_cache}) <- unsafeCompiler $
        runStateT (loadWith e) (emptyStatus (fromMaybe "./" d))
    let imps = mapMaybe (mkDep . chainedImport) (DM.keys _cache)
    compilerTellDependencies imps
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
