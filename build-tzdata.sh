#!/bin/bash

set -e

VER=2014a

base=$(dirname $(readlink -f $0))
cd $base

echo Downloading... >&2
wget -c http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz
wget -c http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz

echo Checking... >&2
sha256sum -c /dev/stdin <<'EOF'
05b93ba541b167a4c10f2e81a7baf972c24ff12db27d85f6c2dd328443c4d3f5  tzcode2014a.tar.gz
7cff254ce85e11b21c994b284bccd1e12ecda9dadf947fbb32e1912fd520e8b1  tzdata2014a.tar.gz
EOF

echo Unpacking... >&2
rm -rf ./tzdist
mkdir tzdist
cd tzdist
tar xzf ../tzcode$VER.tar.gz
tar xzf ../tzdata$VER.tar.gz

echo Building... >&2
make TOPDIR=$base/tzdist/dest install

echo Renaming... >&2
cd $base
rm -rf tzdata
mv tzdist/dest/etc/zoneinfo tzdata
cd tzdata
find . -type f -name '[A-Z]*' -exec mv '{}' '{}.zone' \;
rm localtime posixrules
