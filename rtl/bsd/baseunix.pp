{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Carl Eric Codere development team

    Base Unix unit modelled after POSIX 2001.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit BaseUnix;

Interface

Uses UnixType;

{$i aliasptp.inc}

{$packrecords C}
{$define oldreaddir}		// Keep using readdir system call instead
				// of userland getdents stuff.
{$define usedomain}		// Allow uname with "domain" entry.
				// (which is a GNU extension)
{$define posixworkaround}	// Temporary ugly workaround for signal handler.
				// (mainly until baseunix migration is complete)

{$ifndef FPC_USE_LIBC}
{$define FPC_USE_SYSCALL}
{$endif}

{$i errno.inc}		{ Error numbers }
{$i bunxtype.inc}	{ Types }
{$i ostypes.inc}
{$ifdef FPC_USE_LIBC}
const clib = 'c';
{$i oscdeclh.inc}
{$ELSE}
{$i bunxh.inc}		{ Functions}
{$ENDIF}

{$ifndef ver1_0}
function fpgeterrno:longint; external name 'FPC_SYS_GETERRNO';
procedure fpseterrno(err:longint); external name 'FPC_SYS_SETERRNO';
{$else}
function fpgeterrno:longint; 
procedure fpseterrno(err:longint); 
{$endif}

{$ifndef ver1_0}
property errno : cint read fpgeterrno write fpseterrno;
{$endif}

{$i bunxovlh.inc}

{$ifdef FPC_USE_LIBC}
function settimeofday(tp:ptimeval;tzp:ptimezone):cint; cdecl; external clib name 'settimeofday';
{$else}
function settimeofday(tp:ptimeval;tzp:ptimezone):cint;
{$endif}

implementation

Uses Sysctl;

{$ifndef ver1_0}
//function fpgeterrno:longint; external name 'FPC_SYS_GETERRNO';
//procedure fpseterrno(err:longint); external name 'FPC_SYS_SETERRNO';
{$else}
// workaround for 1.0.10 bugs.

function intgeterrno:longint; external name 'FPC_SYS_GETERRNO';
procedure intseterrno(err:longint); external name 'FPC_SYS_SETERRNO';

function fpgeterrno:longint; 
begin
  fpgeterrno:=intgeterrno;
end;

procedure fpseterrno(err:longint); 
begin
  intseterrno(err);
end;

{$endif}

{$i bunxmain.inc}	{ implementation}
{$i bunxovl.inc}	{ redefs and overloads implementation}
{$i settimeo.inc}
end.

{
  $Log$
  Revision 1.13  2004-12-02 15:11:42  marco
   * initial settimeofday

  Revision 1.12  2004/11/19 13:15:14  marco
   * external rework. Mostly done.

  Revision 1.11  2004/11/14 12:21:08  marco
   * moved some calls from unix to baseunix. Darwin untested.

  Revision 1.10  2004/05/31 18:03:51  jonas
    * moved fpgeterrno/fpseterrno declarations to before their actual usage

  Revision 1.9  2004/03/04 22:15:16  marco
   * UnixType changes. Please report problems to me.

  Revision 1.8  2004/01/04 21:04:08  jonas
    * declare C-library routines as external in libc for Darwin (so we
      generate proper import entries)

  Revision 1.7  2004/01/03 23:56:11  marco
   * fix for 1.0 compability issue

  Revision 1.6  2003/12/30 12:26:21  marco
   * FPC_USE_LIBC

  Revision 1.5  2003/12/10 17:13:43  marco
   * property support under ifndef ver1_0

  Revision 1.4  2003/12/10 17:08:33  marco
   * property errno defined

  Revision 1.3  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.2  2003/06/03 14:23:45  marco
   * Moved prototypes outside of baseunix. And shared with linux for now

  Revision 1.1  2003/01/05 19:01:28  marco
   * FreeBSD compiles now with baseunix mods.

  Revision 1.1  2002/12/18 16:44:09  marco
   * more new RTL

  Revision 1.2  2002/11/14 12:17:28  marco
   * for now.

}
