#!/bin/bash
set -e

if [ "$#" -ne 2 ]; then
  echo "Usage: scripts/hackage-docs.sh VERSION_NUMBER HACKAGE_USER"
  exit 1
fi

pkg=lens
ver=$1
user=$2

if [ ! -f "$pkg.cabal" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

dir=$(mktemp -d build-docs.XXXXXX)
trap 'rm -r "$dir"' EXIT

cabal haddock --hoogle --hyperlink-source --html-location='http://hackage.haskell.org/package/$pkg/docs' --contents-location='http://hackage.haskell.org/package/$pkg'

cp -R dist/doc/html/$pkg/ $dir/$pkg-$ver-docs

tar cvz -C $dir --format=ustar -f $dir/$pkg-$ver-docs.tar.gz $pkg-$ver-docs

curl -X PUT \
     -H 'Content-Type: application/x-tar' \
     -H 'Content-Encoding: gzip' \
     -u "$user" \
     --data-binary "@$dir/$pkg-$ver-docs.tar.gz" \
     "https://hackage.haskell.org/package/$pkg-$ver/docs"
