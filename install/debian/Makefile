#############################################################################
# Root Makefile for Debian Packaging
# Copyright (c) 1998 by the Free Pascal Development Team
#############################################################################

all:
	make -C compiler cycle RELEASE=1
	make -C rtl/linux all RELEASE=1
	make -C rtl/utils all RELEASE=1
#	make -C docs html

install:
	make -C compiler installlib
	make -C rtl/linux install
	make -C rtl/linux libinstall PPUMOVE=../utils/ppumove
	make -C rtl/utils install
#	make -C docs install

clean:
	make -C compiler clean
	make -C rtl/utils clean
	make -C rtl/linux libsclean
#	make -C docs clean
