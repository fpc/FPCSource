{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    Linux (pthreads) threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$define dynpthreads}

unit systhrds;

interface

  uses
    unixtype;
{ Posix compliant definition }

  type
     PRTLCriticalSection = ^TRTLCriticalSection;
     TRTLCriticalSection = pthread_mutex_t;

{ Include generic thread interface }
{$i threadh.inc}

implementation

{*****************************************************************************
                             Generic overloaded
*****************************************************************************}

{ Include generic overloaded routines }
{$i thread.inc}

{ Include OS independent Threadvar initialization }
{$ifdef HASTHREADVAR}
{$i threadvr.inc}
{$endif HASTHREADVAR}

Procedure InitSystemThreads;

begin
  SetNoThreadManager;
end;

initialization
  InitSystemThreads;
end.
{
  $Log$
  Revision 1.22  2004-09-09 20:29:06  jonas
    * fixed definition of pthread_mutex_t for non-linux targets (and for
      linux as well, actually).
    * base libpthread definitions are now in ptypes.inc, included in unixtype
      They sometimes have an extra underscore in front of their name, in
      case they were also exported by the packages/base/pthreads unit, so
      they can keep their original name there
    * cthreadds unit now imports systuils, because it uses exceptions (it
      already did so before as well)
    * fixed many linux definitions of libpthread functions in pthrlinux.inc
      (integer -> cint etc)
    + added culonglong type to ctype.inc

  Revision 1.21  2003/11/26 20:10:59  michael
  + New threadmanager implementation

  Revision 1.20  2003/11/19 10:54:32  marco
   * some simple restructures

  Revision 1.19  2003/11/18 22:36:12  marco
   * Last patch was ok, problem was somewhere else. Moved *BSD part of pthreads to freebsd/pthreads.inc

  Revision 1.18  2003/11/18 22:35:09  marco
   * Last patch was ok, problem was somewhere else. Moved *BSD part of pthreads to freebsd/pthreads.inc

  Revision 1.17  2003/11/17 10:05:51  marco
   * threads for FreeBSD. Not working tho

  Revision 1.16  2003/11/17 08:27:50  marco
   * pthreads based ttread from Johannes Berg

  Revision 1.15  2003/10/01 21:00:09  peter
    * GetCurrentThreadHandle renamed to GetCurrentThreadId

  Revision 1.14  2003/10/01 20:53:08  peter
    * GetCurrentThreadId implemented

  Revision 1.13  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.12  2003/09/16 13:17:03  marco
   * Wat cleanup, ouwe syscalls nu via baseunix e.d.

  Revision 1.11  2003/09/16 13:00:02  marco
   * small BSD gotcha removed (typing mmap params)

  Revision 1.10  2003/09/15 20:08:49  marco
   * small fixes. FreeBSD now cycles

  Revision 1.9  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.8  2003/03/27 17:14:27  armin
  * more platform independent thread routines, needs to be implemented for unix

  Revision 1.7  2003/01/05 19:11:32  marco
   * small changes originating from introduction of Baseunix to FreeBSD

  Revision 1.6  2002/11/11 21:41:06  marco
   * syscall.inc -> syscallo.inc

  Revision 1.5  2002/10/31 13:45:21  carl
    * threadvar.inc -> threadvr.inc

  Revision 1.4  2002/10/26 18:27:52  marco
   * First series POSIX calls commits. Including getcwd.

  Revision 1.3  2002/10/18 18:05:06  marco
   * $I pthread.inc instead of pthreads.inc

  Revision 1.2  2002/10/18 12:19:59  marco
   * Fixes to get the generic *BSD RTL compiling again + fixes for thread
     support. Still problems left in fexpand. (inoutres?) Therefore fixed
     sysposix not yet commited

  Revision 1.1  2002/10/16 06:22:56  michael
  Threads renamed from threads to systhrds

  Revision 1.1  2002/10/14 19:39:17  peter
    * threads unit added for thread support

}

