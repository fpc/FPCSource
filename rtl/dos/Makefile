#
# Minimalist version of a makefile, so make clean at least works.
#
.PHONY: all clean install

all:
	-make -C go32v2 all
	-make -C go32v1 all

clean:
	-make -C go32v2 clean
	-make -C go32v1 clean

install:
	-make -C go32v2 install
	-make -C go32v1 install

