#############################################################################
# Root Makefile for Debian Packaging
# Copyright (c) 1998 by the Free Pascal Development Team
#############################################################################

RELEASE=1 

export RELEASE

all:
	make -C compiler cycle
	make -C rtl/linux all
	make -C rtl/utils all

install:
	make -C compiler installlib
	make -C rtl/linux install
	make -C rtl/linux libinstall PPUMOVE=../utils/ppumove
	make -C rtl/utils install

clean:
	make -C compiler clean
	make -C rtl/utils clean
	make -C rtl/linux libsclean
