
Common *BSD files:

bsdmacro.inc		The POSIX IS_DIR etc macro's.
bsdsysc.inc		The base syscalls for *BSD system unit.
			  including a few that are _not_ posix, but still
			  req'ed in the system unit.
bsdtypes.inc		some non POSIX BSD types required for the
			syscalls
bsduname.inc		The Uname implementation. Requires unit sysctl
osposix.inc		The implementation of unit posix, redirects to libc
			 or bsdtypes.inc (via aliases)
osposixh.inc		The headers of unit posix.
sysctl.pp		Some basic sysctl headers.
sysposix.inc		BSD specific part of the implementation
i386/syscall.inc	The primitives for performing syscalls
i386/syscallh.inc	Headers to syscall.inc

