#
# $Id$
#
# Makefile to create libgdb.a / libgdb.so
#

ifeq ($(GDB_SRC),)
ifeq ($(OS),DJGPP)
GDB_SRC=$(DJDIR)/gnu/gdb-4.18
else
ifeq ($(OS_TARGET),win32)
GDB_SRC=d:/cygwin/cygwin-b20/gnu/gdb-4.18
else
GDB_SRC=/usr/src/gdb-4.18
endif
endif
endif

ifeq ($(GDB_OBJ),)
GDB_OBJ=$(GDB_SRC)
endif

ifeq ($(OS)),DJGPP)
OS_CFLAGS=-DMAINTENANCE_CMDS=0 -DHAVE_CONFIG_H -O2 -DHAVE_LONG_DOUBLE=1 \
	  -DPRINTF_HAS_LONG_DOUBLE=1
OS_FLAGS=
MMALLOC_INC=
MMALLOC_CFLAGS=-DNO_MMALLOC -DNO_MMALLOC_CHECK
MMALLOC_LIB=
MMALLOC_LIB_DIR=
OS_FLAGS+=MMALLOC=$(MMALLOC) MMALLOC_CFLAGS="$(MMALLOC_CFLAGS)"
CHECKS=check_echo check_awk
EXCLUDE_REGEX=
else
OS_CFLAGS=-DMAINTENANCE_CMDS=1 -DHAVE_CONFIG_H -O2 -DNO_MMALLOC
#MMALLOC=../mmalloc/libmmalloc.a
#MMALLOC_INC=o_gdb/mmalloc s_gdb/mmalloc
#CHECKS=
#EXCLUDE_REGEX=| sed -e '/.*gnu-regex.*/d'
endif

MMALLOC_LIB=$(subst ..,$(GDB_OBJ),$(MMALLOC))
MMALLOC_LIB_DIR=$(subst /libmmalloc.a,,$(MMALLOC_LIB))

vpath %.a $(GDB_OBJ)/bfd $(GDB_OBJ)/intl $(GDB_OBJ)/libiberty $(GDB_OBJ)/opcodes $(GDB_OBJ)/readline $(MMALLOC_LIB_DIR)

_INCS=o_gdb/gdb s_gdb/gdb s_gdb/include s_gdb/gdb/config \
	o_gdb/bfd s_gdb/bfd \
	o_gdb/opcodes s_gdb/opcodes \
	o_gdb/intl s_gdb/intl \
	o_gdb/readline s_gdb/readline \
	o_gdb/libiberty s_gdb/libiberty \
	$(MMALLOC_INC)

INCS=$(subst s_gdb,$(GDB_SRC),$(subst o_gdb,$(GDB_OBJ),$(_INCS)))

INC_PATH=$(addprefix -I,$(INCS))

all:: libgdb.a

libgdb.c: Makefile
	echo '#include <defs.h>' > libgdb.c
	echo '#include <breakpoint.h>' >> libgdb.c
	echo '#include <symtab.h>' >> libgdb.c
	echo '#include <symfile.h>' >> libgdb.c
	echo '#include <objfiles.h>' >> libgdb.c
	echo '#include <inferior.h>' >> libgdb.c
	echo '#include <gnu-regex.h>' >> libgdb.c
	echo '#include <expression.h>' >> libgdb.c
	echo '#include <language.h>' >> libgdb.c
	echo '#include <complaints.h>' >> libgdb.c
	echo '#include <coff/internal.h>' >> libgdb.c
	echo '#include <libcoff.h>' >> libgdb.c
	echo '#include <ctype.h>' >> libgdb.c
	echo '#include <string.h>' >> libgdb.c
	echo '#include <buildsym.h>' >> libgdb.c
	echo '#include <gdb-stabs.h>' >> libgdb.c
	echo '#include <stabsread.h>' >> libgdb.c
	echo '#include <gdbcmd.h>' >> libgdb.c
	echo '#include <setjmp.h>' >> libgdb.c
	echo '#include <demangle.h>' >> libgdb.c
	echo '#include <annotate.h>' >> libgdb.c
	echo '#include <top.h>' >> libgdb.c

LIBGDB_FLAGS+=$(OS_FLAGS)
CFLAGS+=$(OS_CFLAGS)

$(GDB_OBJ)/bfd/bfd.h:
	$(MAKE) -C $(dir $@) $(notdir $@)

ifneq ($(LIBGDB_H),)
libgdb.h: $(LIBGDB_H)
	ln -sf $(LIBGDB_H) libgdb.h
else
libgdb.h: $(GDB_OBJ)/bfd/bfd.h libgdb.c
	gcc -E -P $(INC_PATH) $(CFLAGS) $(MMALLOC_CFLAGS) libgdb.c \
	  | sed -e '/^[ 	]*$$/d' > libgdb.h
	gcc -E -P -dM $(INC_PATH) $(CFLAGS) $(MMALLOC_CFLAGS) libgdb.c \
	  | sed -e '/^[ 	]*$$/d' >> libgdb.h
endif

gdb.t: $(GDB_OBJ)/gdb/libgdb-files
	printf "%s\n" $(shell cat $<) | sed -n -e 's,^.*\.o,$(dir $<)&,p' $(EXCLUDE_REGEX) > $@

%.t: %.a
	ar t $< | sed -e 's,^.*\.o,$(dir $<)&,' > $@

libbfd.t: $(GDB_OBJ)/bfd/libbfd.a
libintl.t: $(GDB_OBJ)/intl/libintl.a
libiberty.t: $(GDB_OBJ)/libiberty/libiberty.a
libopcodes.t: $(GDB_OBJ)/opcodes/libopcodes.a
libreadline.t: $(GDB_OBJ)/readline/libreadline.a
libmmalloc.t: $(GDB_OBJ)/mmalloc/libmmalloc.a

$(GDB_OBJ)/%.a:
	$(MAKE) -C $(dir $@) CFLAGS="$(CFLAGS)" $(LIBGDB_FLAGS)

$(GDB_OBJ)/gdb/libgdb-files:
# this is needed, because the file is part of the src distrib, but
# the object files not (at least in DJGPP)
	rm -f $@
	$(MAKE) -C $(dir $@) $(notdir $@) CFLAGS="$(CFLAGS)" $(LIBGDB_FLAGS)

T_FILES= libbfd.t libiberty.t libintl.t libopcodes.t libreadline.t $(notdir $(subst .a,.t,$(MMALLOC_LIB))) gdb.t

list: $(T_FILES)

libgdb.a: $(CHECKS) list
	rm -f libgdb.a
# do not use any xmalloc.o, because it is defined in utils.o
	ar rcs $@ $(shell cat $(T_FILES) | sed -e '/.*xmalloc.*/d')
	rm -f $(T_FILES)

libgdb.so: $(CHECKS) list
	rm -f libgdb.so
# do not use any xmalloc.o, because it is defined in utils.o
	ld -shared -o $@ $(shell cat $(T_FILES) | sed -e '/.*xmalloc.*/d')
	rm -f $(T_FILES)

lib.zip : libgdb.a
	zip lib libgdb.a

