POSIX directory information
---------------------------
This directory contains the system call interface to
POSIX compliant systems. These files should be
completed by OS-specific files. This permits to
easily create  common and maintanable base
runtime library units (such as dos and system).

Limitations:
------------
- Only single byte character sets are supported (ASCII, ISO8859-1)
- Path and filenames are limited to 255 characters 
  (shortstrings are limited to 255 characters)

Files in this directory

posix.tem
----------
Posix unit template.

dos.pp
------
POSIX compliant dos unit. The following routines
and variables must be implemented / declared on
a platform by platform basis:

DiskFree()
DiskSize()
DosVersion()
GetTimeZoneString(): Should return the string of
the timezone information , as defined by POSIX,
if not available, should return an empty string.
This string is usually stored in the 'TZ' environment
variable.
GetTimeZoneFileName() : Should return the absolute path to 
the timezone filename to use to convert the UTC time to 
local time. The format of the timezone files are those 
specific in glibc.
FixDriveStr : Array of pchar which contains
the names of the fixed disks :
(index 0 : current directory
 index 1 : first floppy disk
 index 2 : second floppy disk
 index 3 : boot disk
)


sysposix.inc
------------
Most of the specific operating system
routines which can be implemented using
the POSIX interface are implemented in
this unit. This should be included in
the target operating system system unit
to create a complete system unit.

Files required to port the compiler to a POSIX
compliant system (should reside in the target
OS directory):

  osposixh.inc : This includes all constants,
  type definitions and structures used
  (this is operating system dependant), except
  for those related to signals. It includes
  the signal file.

  osposix.inc : The actuall system call routines
  to the routine prototypes defined in posixh.inc.
  (either these can be an interface to a libc, or
  actual system calls).

  errno.inc : All possible error codes which
  can be returned by the operating system.
  
  signal.inc : Defines all constants and types
  related to signals, it must at least define
  the POSIX signal types and constants, but
  can also add other OS specific types and
  constants related to signals.


Templates for the osposix.inc file (when linked
with the GNU libc), errno.inc and osposixh.inc
are included in this directory and have the 
extension .tem . They should be used as a basis 
to port to a new operating system.

When sysposix.inc is used, the following system
unit routines must be implemented for each new
operating system, as they are not reproducable
under the POSIX interface:

 function sbrk(size : longint): longint;
 procedure do_truncate (handle,pos:longint);
 function do_isdevice(handle:longint):boolean;

When dos.pp is used, the following dos
unit routines must be implemented for each new
operating system, as they are not reproducable
under the POSIX interface:

 function diskfree(drive : byte) : int64;
 function disksize(drive: byte) : int64;





