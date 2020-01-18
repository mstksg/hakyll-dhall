{ foo =
    ./foo.dhall
, bar =
    < Hey | Ho >.Hey
, baz =
    let not =
          https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/Bool/not sha256:723df402df24377d8a853afed08d9d69a0a6d86e2e5b2bac8960b0d4756c7dc4

    in  not True
, hi =
    ~/local/local-test.dhall
}
