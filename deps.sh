#!/usr/bin/env bash

# Update harfbuzz
rm -rf harfbuzz/
UUID=$(uuidgen)
git clone --depth=1 https://github.com/behdad/harfbuzz.git "/tmp/$UUID"
mkdir harfbuzz
mv "/tmp/$UUID/src" harfbuzz/
rm -rf "/tmp/$UUID"

# Update ICU
rm -rf icu/
UUID=$(uuidgen)
git clone --depth=1 https://github.com/unicode-org/icu.git "/tmp/$UUID"
mkdir icu
mv "/tmp/$UUID/icu4c/source/common" icu/
mv "/tmp/$UUID/icu4c/source/stubdata/stubdata.*" icu/stubdata/
rm -rf "/tmp/$UUID"

# Update woff2/brotli
rm -rf woff2
UUID=$(uuidgen)
git clone --depth=1 --recursive https://github.com/google/woff2.git "/tmp/$UUID"
mkdir -p woff2/brotli/c
mv "/tmp/$UUID/brotli/c/common" woff2/brotli/c/
mv "/tmp/$UUID/brotli/c/dec" woff2/brotli/c/
mv "/tmp/$UUID/brotli/c/include" woff2/brotli/c/
mv "/tmp/$UUID/src" woff2/
mv "/tmp/$UUID/include" woff2/
rm -rf "/tmp/$UUID"

