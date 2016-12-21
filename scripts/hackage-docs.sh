#!/bin/bash

# Run this script in the top-level of your package directory
# (where the .cabal file is) to compile documentation and
# upload it to hackage.

# Requirements:
# cabal-install-1.24 (for --for-hackage)
# haddock 2.17 (for the hyperlinked source)

set -e

dir=$(mktemp -d dist-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal configure --builddir="$dir" --extra-lib-dirs=/usr/local/Cellar/openssl/1.0.2j/lib --extra-include-dirs=/usr/local/Cellar/openssl/1.0.2j/include
cabal haddock --builddir="$dir" --for-hackage --haddock-option=--hyperlinked-source
cabal upload -d $dir/*-docs.tar.gz
