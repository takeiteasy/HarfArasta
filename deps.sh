#!/usr/bin/env bash

# Update harfbuzz
rm -rf harfbuzz/
git clone https://github.com/behdad/harfbuzz.git
rm -rf harfbuzz/tests

# Update ICU
rm -rf icu/
git clone https://github.com/unicode-org/icu.git
mv icu/icu4c/source/common /tmp
mkdir /tmp/stubdata
mv icu/icu4c/source/stubdata/stubdata.* /tmp/stubdata
rm -rf icu/* icu/.*
mv /tmp/common icu/
mv /tmp/stubdata icu/