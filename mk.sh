 #! /bin/sh
set -e
FPCMAKE=/usr/local/bin/fpcmake

find . -name Makefile.fpc | xargs $FPCMAKE -Tall 

cd ./rtl
./regenmakefiles.sh
cd ./..

cd ./packages
./regenmakefiles.sh
cd ./..

cd ./utils/build/
$FPCMAKE -Tall -s -o Makefile.pkg Makefile.fpc
cd ./../..

cd ./packages/build/
$FPCMAKE -Tall -s -o Makefile.pkg Makefile.fpc
cd ./../..