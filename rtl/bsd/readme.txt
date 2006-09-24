{
    $Id: readme.txt,v 1.2 2002/10/27 11:58:30 marco Exp $
    This file is part of the Free Pascal run time librar~y.
    Copyright (c) 2000 by Marco van de Voort
    member of the Free Pascal development team.

    Filellist and some notes about the *BSD RTL architecture.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

*BSD commonly means FreeBSD, OpenBSD and NetBSD, but since Apple's Darwin
	has a FreeBSD userland, I also add Darwin to it. At least Darwin's
	userland seems to be compatible enough to be included, despite its
	internal Mach architecture.

Common *BSD files:

bsd.pa		BSD specific syscall functions
bsdmacro.inc		The POSIX mode_t (IS_DIR etc) and exit macro's.
bsdsysc.inc		The base syscalls for *BSD system unit.
			  including a few that are _not_ posix, but still
			  required in the system unit. All routines have
			  a public alias.
bsdsysch.inc		EXTERNAL declarations for the non-posix calls in
			bsdsysc.inc (to import them into e.g. Unix)
bsdtypes.inc		some non POSIX BSD types required for the
			syscalls and base functions.
bsdfuncs.inc		POSIX syscalls and functions that are not needed
			for system.
osposix.inc		The implementation of unit posix, redirects to libc
			 or bsdtypes.inc (via aliases)
osposixh.inc		The headers of unit posix.
sysctl.pp		Some basic sysctl headers, needed for implementation
			of POSIX functions.
sysposix.inc		BSD specific part of the implementation
i386/syscall.inc	The primitives for performing syscalls
i386/syscallh.inc	Headers to syscall.inc
powerpc/syscall.inc	likewise for PPC.

