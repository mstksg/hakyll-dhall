# [hakyll-dhall][]

[![hakyll-dhall on Hackage](https://img.shields.io/hackage/v/hakyll-dhall.svg?maxAge=86400)](https://hackage.haskell.org/package/hakyll-dhall)
[![Build Status](https://travis-ci.org/mstksg/hakyll-dhall.svg?branch=master)](https://travis-ci.org/mstksg/hakyll-dhall)

[Hakyll][hakyll] compiler and loader for [Dhall][dhall] files.  Functions are
intended to track all local dependencies within the project directory, so
rebuilds are properly triggered on up-stream imports.  Provides options for
customizing rebuilding behavior for network, environment variable, and
non-project local files.

`loadDhall` and `loadDhallExpr` allow for loading and parsing of Dhall files
for usage within the 'Compiler' monad, so you can use the results as
intermediate parts in building your pages.  `parseDhall` allows directly
passing in Dhall strings to parse and resolve, tracking imports.
`dhallCompiler` is meant as a "final end-point", which just pretty-prints a
parsed Dhall file, with optional normalization.

Hakyll compiler and loader for Dhall files.  Functions are intended to
track all local dependencies within the project directory, so rebuilds
are properly triggered on up-stream imports.  Provides options for
customizing rebuilding behavior for network, environment variable, and
non-project local files.

There are three major workflows:

1. `dExprCompiler`, `loadDhall`, and `dhallCompiler`, for loading underlying
   Dhall files, saving them into the Hakyll cache and later interpreting them
   as values.

2. `parseDhall` and `parseDhallExpr`, for parsing Dhall expressions provided as
   strings, and resolving them while tracking dependencies.

3. `dhallPrettyCompiler`, for processing and re-formatting Dhall files and
   presenting them as-is as a "final end-point".

[hakyll-dhall]: http://hackage.haskell.org/package/hakyll-dhall
[hakyll]: http://hackage.haskell.org/package/hakyll
[dhall]: http://hackage.haskell.org/package/dhall
