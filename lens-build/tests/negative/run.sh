#!/bin/sh
# Negative tests for lens-build: each fixture below MUST fail to compile, with a
# specific custom message. For each one we build the gated executable and assert
# both that the build FAILS and that the failure is our message (not some
# unrelated error). Run from the repository root.
set -u

fail=0

# check <executable> <expected substring of the error message>
check() {
  target=$1
  expected=$2
  log=$(cabal build -fbuild-negative-test "lens-build:exe:$target" 2>&1)
  if [ $? -eq 0 ]; then
    printf '%s\n' "$log"
    echo "FAILED [$target]: compiled, but it must be rejected."
    fail=1
    return
  fi
  if printf '%s\n' "$log" | grep -q "$expected"; then
    echo "ok [$target]: rejected with '$expected'"
  else
    printf '%s\n' "$log"
    echo "FAILED [$target]: compilation failed, but without the expected message '$expected'."
    fail=1
  fi
}

# Builder usage: an incomplete build.
check t-missing-field 'missing field'
# makeBuild generator guards on unsupported input.
check t-sum-type      'single-constructor records only'
check t-positional    'named fields is required'
check t-parameterized 'no type parameters'

if [ "$fail" -eq 0 ]; then
  echo 'negative tests: all ok'
  exit 0
fi
echo 'negative tests: FAILED'
exit 1
