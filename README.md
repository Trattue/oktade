# oktade

Parser for JVM classfiles, built upon the parser combinator library
[attoparsec](https://hackage.haskell.org/package/attoparsec). Classfile versions
targeted for correct parsing are the same as of JDK for Java SE 17: 45 - 61
(which corresponds to Java SE 1.0.2 - 17).

**This software is still in early development and not ready for serious usage
yet. Development is mainly happening on the [dev branch](/../../tree/dev)**

Oktade is able to parse and write entire classfiles correctly, however some
things still need to be worked on:
* Finish Argument parsing. Currently, many arguments are represented as
bytestrings in their most general form possible.
  * Bytecode parsing.
* Attribute validation: disallow multiple attributes of the same type if the
type only allows one instance per classfile
* Signature string parsing. Currently, signatures are stored as bytestrings.
* MUTF-8 decoding and encoding. (If that is even necessary, did not have any
issues with that so far...)
* Implement operations for modifying the classfile/bytecode.

## Usage

The main executable can be used in a similar fashion to `javap`: Run
`./oktade <classfile>` to have the parsed classfile printed out.

## Development
### Prerequisites

A working installation of Haskell (GHC 8.10.7 or more recent) and cabal
(3.6.2.0 or more recent). The easiest way of obtaining those is using
[GHCup](https://www.haskell.org/ghcup/).

During development, try maintaining compatability with the GHC version marked as
'recommended' in GHCup (currently GHC 8.10.7).

### Development Environment Setup

Clone this project via `git clone https://github.com/Trattue/oktade` or using
your git client.

### Building and Testing

Run `cabal build` to produce executables of all targets. This will produce
executables of the main application, the test application and the benchmark
application.

For testing, run `cabal test --test-show-details=direct` afterwards.

For benchmarking, run `cabal bench`. This will benchmark oktade parsing and
unparsing every classfile in the cfr-tests project for 200 iterations (after a
warmup of 10 iterations). You can find more information on how the benchmark
works in the files in the [testsuite/benchmarks/](testsuite/benchmarks/) folder.

### Code Style

Use the [ormolu formatter](https://github.com/tweag/ormolu); maximum line length
is 80 (except for stuff like URIs). Make sure to follow all of
[hlint](https://github.com/ndmitchell/hlint)'s suggestions.

For the [.cabal file](oktade.cabal), use `cabal format`.

## License

Copyright (c) 2021 Trattue

Oktade is distributed under the terms of the Apache License (Version 2.0), see
[LICENSE](LICENSE) for details.

The oktade repository includes code from other projects which may be licensed
under other terms and conditions:
* [cfr_tests](https://github.com/leibnitz27/cfr_tests): Licensed under the MIT
  License license (see [testsuite/tests/LICENSE](testsuite/tests/LICENSE) for
  details).<br>
  Compiled classes from the project can be found in the
  [testsuite/tests/](testsuite/tests/) directory.<br>
  Thanks to the [CFR](https://github.com/leibnitz27/cfr) developers for making
  their comprehensive test suite open source!

Additionally, this project uses third party libraries. You can find a list of
those in the [.cabal file](oktade.cabal). While building oktade, your compiler
may link those libraries to oktade; please check their corresponding licenses
for legal infomation, e.g. regarding distribution of binaries.
