{
    $Id$
    Copyright (c) 2000-2002 by Marco van de Voort

    Some non POSIX BSD types used internally in the system unit.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

Type

  timespec = packed record
    tv_sec   : time_t;
    tv_nsec  : clong;
  end;
  ptimespec= ^timespec;

  timezone = packed record
    minuteswest,
    dsttime  : cint;
  end;
  ptimezone =^timezone;
  TTimeZone = timezone;

  rusage = packed record
	ru_utime    : timeval;		{ user time used }
	ru_stime    : timeval;		{ system time used }
	ru_maxrss   : clong;		{ max resident set size }
	ru_ixrss    : clong;		{ integral shared memory size }
	ru_idrss    : clong;		{ integral unshared data " }
	ru_isrss    : clong;		{ integral unshared stack " }
	ru_minflt   : clong;		{ page reclaims }
	ru_majflt   : clong;		{ page faults }
	ru_nswap    : clong;		{ swaps }
	ru_inblock  : clong;		{ block input operations }
	ru_oublock  : clong;		{ block output operations }
	ru_msgsnd   : clong;		{ messages sent }
	ru_msgrcv   : clong;		{ messages received }
	ru_nsignals : clong;		{ signals received }
	ru_nvcsw    : clong;		{ voluntary context switches }
	ru_nivcsw   : clong;		{ involuntary " }
	end;
// #define	ru_last		ru_nivcsw
// #define	ru_first	ru_ixrss

{
 $Log$
 Revision 1.3  2003-09-15 07:23:51  marco
  * fix

 Revision 1.2  2003/09/14 20:15:01  marco
  * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

 Revision 1.1  2003/01/05 19:01:28  marco
  * FreeBSD compiles now with baseunix mods.

 Revision 1.4  2002/10/27 17:21:29  marco
  * Only "difficult" functions + execvp + termios + rewinddir left to do

 Revision 1.3  2002/10/27 11:58:30  marco
  * Modifications from Saturday.

 Revision 1.2  2002/09/07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.1  2002/08/19 12:29:11  marco
  * First working POSIX *BSD system unit.


}
