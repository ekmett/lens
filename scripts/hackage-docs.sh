#!/bin/bash
set -e

if [ "$#" -ne 1 ]; then
  echo "Usage: scripts/hackage-docs.sh HACKAGE_USER"
  exit 1
fi

cabal_file=$(find . -name "*.cabal" -maxdepth 1 -print -quit)
if [ ! -f "$cabal_file" ]; then
  echo "Run this script in the top-level package directory"
  exit 1
fi

pkg=$(awk -F ":[[:space:]]*" '$1=="name"    { print $2 }' < "$cabal_file")
ver=$(awk -F ":[[:space:]]*" '$1=="version" { print $2 }' < "$cabal_file")

if [ -z "$pkg" ]; then
  echo "Unable to determine package name"
  exit 1
fi

if [ -z "$ver" ]; then
  echo "Unable to determine package version"
  exit 1
fi

user=$1

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
