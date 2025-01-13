# LargeHashable

[![BuildStatus](https://github.com/factisresearch/large-hashable/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/factisresearch/large-hashable/actions/workflows/haskell-ci.yml/badge.svg?branch=master)
[![Hackage](https://img.shields.io/hackage/v/large-hashable.svg)](http://hackage.haskell.org/package/large-hashable)

Efficiently hash Haskell values with MD5, SHA256, SHA512 and other
hashing algorithms.

## Install

* Using cabal: `cabal install large-hashable`
* Using Stack: `stack install large-hashable`

### Building from git repository

- clone the repository
- Install the stack tool (http://haskellstack.org)
- `stack build` builds the code
- `stack test` builds the code and runs the tests
- `run-benchmarks.sh` runs the benchmark suite
