#
# Minimalist version of a makefile, so make clean at least works.
#

# Warning: this file contains TAB (#9) characters that are required for
# make. Make sure you use an editor that does not replace TABs with
# spaces, or the makefile won't work anymore after you save.

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

