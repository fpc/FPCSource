
This is the common directory for RTL code which is shared among Amiga-like
platforms. That means AmigaOS on 68k, MorphOS and AROS on i386 currently.

All code which is committed to this directory must be compilable and working
on these systems. System specific implementations are possible to add via
include files, which are then included into these files at the right place.

  Karoly Balogh (2014.08.22)
