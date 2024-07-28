#!/bin/bash
./compiler/ppcrossmipsel ./rtl/ps1/system.pp -O- -Tps1 -Fi./rtl/mipsel -Fi./rtl/inc -Fi./rtl/ps1 -a -XP/usr/local/mipsel-unknown-elf/bin/mipsel-unknown-elf- -Cfnone
./compiler/ppcrossmipsel ./rtl/inc/fpintres.pp -O- -Tps1 -Fu./rtl/ps1 -Fi./rtl/mipsel -Fi./rtl/inc -Fi./rtl/ps1 -a -XP/usr/local/mipsel-unknown-elf/bin/mipsel-unknown-elf- -Cfnone 
./compiler/ppcrossmipsel ./rtl/ps1/si_prc.pp -O- -Tps1 -Fu./rtl/ps1 -Fi./rtl/mipsel -Fi./rtl/inc -Fi./rtl/ps1 -a -XP/usr/local/mipsel-unknown-elf/bin/mipsel-unknown-elf- -Cfnone 
#./compiler/ppcrossmipsel ./rtl/ps1/objpas.pp -O- -Tps1 -Fu./rtl/ps1 -Fi./rtl/mipsel -Fi./rtl/inc -Fi./rtl/ps1 -a -XP/usr/local/mipsel-unknown-elf/bin/mipsel-unknown-elf- -Cfnone 
#
./compiler/ppcrossmipsel ./rtl/objpas/objpas.pp -O- -Tps1 -Fu./rtl/ps1 -Fi./rtl/mipsel -Fi./rtl/inc -Fi./rtl/ps1 -a -XP/usr/local/mipsel-unknown-elf/bin/mipsel-unknown-elf- -Cfnone 