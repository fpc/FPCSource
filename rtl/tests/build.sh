#!/bin/bash
exec /home/michael/fpc/compiler/ppcx64 -vwhn testextrtti.pp -Fu../units/x86_64-linux -gl $*
