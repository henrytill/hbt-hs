# hbt-hs

[![CI](https://github.com/henrytill/hbt-hs/actions/workflows/ci.yml/badge.svg)](https://github.com/henrytill/hbt-hs/actions/workflows/ci.yml)

## Conformance

`test/data/` is [hbt-data](https://github.com/henrytill/hbt-data) as a git submodule: the corpus shared with the [Go](https://github.com/henrytill/hbt-go), [OCaml](https://github.com/henrytill/hbt-ocaml) and [Rust](https://github.com/henrytill/hbt-rs) implementations, and the conformance harness that holds each of them to it by running every fixture through the built `hbt` and comparing what it writes. Clone with `--recurse-submodules`.

`nix flake check` runs the harness against the Nix-built executable: `test/data/` is also the `hbt-data` flake input, whose `lib.check` does the work. In the dev shell, which provides the harness's Python:

```sh
cabal build hbt-cli
(cd test/data && python3 -m hbt.conformance --binary "$(cabal list-bin hbt)")                 # every fixture
(cd test/data && python3 -m hbt.conformance --binary "$(cabal list-bin hbt)" markdown/basic)  # a name, substring or glob
```

The harness's flags, what counts as a match, and its timezone policy are documented in `test/data/README.md`. A fixture is added or changed in hbt-data, then picked up here by bumping the submodule.
