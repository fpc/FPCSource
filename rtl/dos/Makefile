#
# Minimalist version of a makefile, so make clean at least works.
#
.PHONY: clean install

clean:
	-$(MAKE) -C go32v2 clean
	-$(MAKE) -C go32v1 clean

install:
	-$(MAKE) -C go32v2 install
	-$(MAKE) -C go32v1 install

