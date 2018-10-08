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
-- 'loadDhall' and 'loadDhallExpr' allow for loading and parsing of Dhall
-- files for usage within the 'Compiler' monad, so you can use the results
-- as intermediate parts in building your pages.  'parseDhall' allows
-- directly passing in Dhall strings to parse and resolve, tracking
-- imports.  'dhallCompiler' is meant as a "final end-point", which just
-- pretty-prints a parsed Dhall file, with optional normalization.


module Hakyll.Web.Dhall (
  -- * Configuration and Options
    DhallCompilerOptions(..), DhallCompilerTrust(..)
  , defaultDhallCompilerOptions, dcoResolver, dcoMinimize, dcoNormalize
  -- ** Resolver Behaviors
  , DhallResolver(..), DefaultDhallResolver(..), drRemap, drFull
  -- * Load Dhall Files
  -- ** As as custom Haskell types
  , loadDhall, loadDhallWith
  -- ** As raw expressions
  , loadDhallExpr, loadDhallExprWith
  , DExpr(..)
  -- * Parse raw Dhall expressions
  , parseDhall, parseDhallWith
  -- * Compile (prettify, normalize, re-map) Dhall Files
  , dhallCompiler
  , dhallRawCompiler, dhallFullCompiler
  , dhallCompilerWith
  -- * Internal Utilities
  , parseRawDhallWith
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
import           Dhall hiding                          (maybe)
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
      -- ^ Strictly for usage with 'dhallCompiler' and family: should the
      -- result be "minimized" (all in one line) or pretty-printed for
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
-- 'dhallRawCompiler'  = 'dhallCompiler' \@'Import'
-- 'dhallFullCompiler' = 'dhallCompiler' \@'X'
-- @
--
-- It might be more convenient to just use 'dhallRawCompiler' or
-- 'dhallFullCompiler'.
dhallCompiler
    :: forall a. (DefaultDhallResolver a, PP.Pretty a)
    => Compiler (Item String)
dhallCompiler = dhallCompilerWith @a defaultDhallCompilerOptions

-- TODO: way to only resolve Env and Absolute and Home?
-- Need to somehow hook into 'loadWith' so it can be recursive

-- | Compile the Dhall file as text according to default
-- 'DhallCompilerOptions' while leaving all imports unchanged and
-- unresolved.  Essentially a Dhall pretty-printer, (optional) normalizer,
-- and re-formatter.
dhallRawCompiler :: Compiler (Item String)
dhallRawCompiler = dhallCompilerWith @Import defaultDhallCompilerOptions

-- | Compile the Dhall file as text according to default
-- 'DhallCompilerOptions', resolving all imports in IO and tracking
-- dependencies.  Essentially a Dhall pretty-printer, (optional)
-- normalizer, and re-formatter.
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
    e <- parseDhallWith dco (Just i) b
    makeItem $ T.unpack (disp e)
  where
    disp
      | _dcoMinimize dco = pretty
      | otherwise        = PP.renderStrict
                         . PP.layoutSmart layoutOpts
                         . PP.unAnnotate
                         . prettyExpr

-- | Version of 'parseDhallWith' that only acceps the 'DRRaw' resolver,
-- remapping the imports with the function in the 'DRRaw'.  Does not
-- perform any normalization.
parseRawDhallWith
    :: DhallCompilerOptions Import
    -> Maybe Identifier
    -> T.Text
    -> Compiler (Expr Src Import)
parseRawDhallWith DCO{..} i b =
    case exprFromText (maybe "Raw dhall string" toFilePath i) b of
      Left  e -> throwError . (:[]) $
        "Error parsing raw dhall file: " ++ show e
      Right e -> join <$> traverse (_drRemap _dcoResolver) e

-- | Parse a Dhall source.  Meant to be useful for patterns similar to
-- @dhall-to-text@.  If using examples from
-- <https://github.com/dhall-lang/dhall-text>, you can use:
--
-- @
-- 'parseDhall' 'Nothing' ".\/make-items .\/people"
-- @
--
-- Any local dependencies within the project directory (./make-items and
-- ./people above, for example) are tracked by Hakyll, and so modifications
-- to required files will also cause upstream files to be rebuilt.
parseDhall
    :: DefaultDhallResolver a
    => Maybe Identifier             -- ^ Optional 'Identifier' used to specify directory root for imports
    -> T.Text
    -> Compiler (Expr Src a)
parseDhall = parseDhallWith defaultDhallCompilerOptions

-- | Version of 'parseDhall' taking custom 'DhallCompilerOptions'.
parseDhallWith
    :: DhallCompilerOptions a
    -> Maybe Identifier             -- ^ Optional 'Identifier' used to specify directory root for imports
    -> T.Text
    -> Compiler (Expr Src a)
parseDhallWith dco i b = case _dcoResolver dco of
    DRRaw  _ -> norm <$> parseRawDhallWith dco i b
    DRFull _ -> fmap norm
              . resolveDhallImports dco i
            =<< parseRawDhallWith (dco { _dcoResolver = defaultDhallResolver })
                  i b
  where
    norm :: Eq b => Expr s b -> Expr s b
    norm
      | _dcoNormalize dco = normalize
      | otherwise         = id

-- | Resolve all imports in a parsed Dhall expression.
--
-- This implements the "magic" of dependency tracking: implemented so that
-- any local dependencies within the project directory are tracked by
-- Hakyll, and so modifications to required files will also cause upstream
-- files to be rebuilt.
resolveDhallImports
    :: DhallCompilerOptions X
    -> Maybe Identifier             -- ^ Optional 'Identifier' used to specify directory root for imports
    -> Expr Src Import
    -> Compiler (Expr Src X)
resolveDhallImports DCO{..} ident e = do
    (res, imps) <- unsafeCompiler $ do
      iRef <- newIORef []
      res <- evalStateT (loadWith e) $
        emptyStatus (takeDirectory (maybe "./" toFilePath ident))
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
    Item i <$> parseDhallWith dco (Just i) b

-- | Load a value of type @a@ that is parsed from a Dhall file at the given
-- 'Identifier'.  Tracks dependencies within project.
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
