#!/bin/bash

set -e

VER=2014b

base=$(dirname $(readlink -f $0))
cd $base

echo Downloading... >&2
wget -c http://www.iana.org/time-zones/repository/releases/tzdata$VER.tar.gz
wget -c http://www.iana.org/time-zones/repository/releases/tzcode$VER.tar.gz

echo Checking... >&2
sha256sum -c /dev/stdin <<EOF
b5dad3b0c7880e3fc030f87354b93776d9c0e761a56f363d917ab02e1754a78b  tzcode$VER.tar.gz
d1e1f3e0c253a8ac0b35ffe27ef95f6366c37beb953391806cd922efce2b1cb5  tzdata$VER.tar.gz
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

echo Building symlinked zoneinfo for compilation... >&2
cd $base/tzdist
make clean
make TOPDIR=$base/tzdist/dest CFLAGS=-DHAVE_LINK=0 install
zdir=$base/tzdist/dest/etc/zoneinfo
# We don't want these:
rm -f $zdir/* || true
rm -rf $zdir/Etc

echo Compiling the tool... >&2
cd $base/tools
ghc -Wall -O --make -package-db ../dist/package.conf.inplace -XHaskell2010 genZones

echo Creating All.hs... >&2
cd $base
./tools/genZones tzdist/dest/etc/zoneinfo/ Data/Time/Zones/All.hs.template Data/Time/Zones/All.hs
