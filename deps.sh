#!/usr/bin/env bash

# Update harfbuzz
rm -rf harfbuzz/
git clone --depth=1 https://github.com/behdad/harfbuzz.git
rm -rf harfbuzz/tests
rm -rf harfbuzz/.git

# Update ICU
rm -rf icu/
git clone --depth=1 https://github.com/unicode-org/icu.git
mv icu/icu4c/source/common /tmp
mkdir /tmp/stubdata
mv icu/icu4c/source/stubdata/stubdata.* /tmp/stubdata
rm -rf icu/* icu/.*
mv /tmp/common icu/
mv /tmp/stubdata icu/
rm -rf icu/.git

# Update woff2/brotli
rm -rf woff2
git clone --depth=1 --recursive https://github.com/google/woff2.git

