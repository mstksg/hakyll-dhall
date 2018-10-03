{ foo = ./foo.dhall
, bar = < Hey = {=} | Ho : {} >
, baz = let not = https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/Bool/not
        in  not True
, hi  = ~/local/local-test.dhall
}
