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
{$ifdef FPC_USE_LIBC}
const clib = 'c';
{$i oscdeclh.inc}
{$ELSE}
{$i bunxh.inc}          { Functions}
{$ENDIF}

{$i bunxovlh.inc}

function fpgeterrno:longint;
procedure fpseterrno(err:longint);

{$ifdef HASGLOBALPROPERTY}
property errno : cint read fpgeterrno write fpseterrno;
{$endif}

implementation

{$i bunxmain.inc}	{ implementation}
{$i bunxovl.inc}	{ redefs and overloads implementation}

{$ifdef ver1_0}
// MvdV 1.0 is buggy in calling externals it seems. dunno what exactly
function intfpgeterrno:longint; external name 'FPC_SYS_GETERRNO';
procedure intfpseterrno(err:longint); external name 'FPC_SYS_SETERRNO';

function fpgeterrno:longint; 

begin
  fpgeterrno:=intfpgeterrno;
end;

procedure fpseterrno(err:longint); 
begin
   intfpseterrno(err);
end;
{$else}
function fpgeterrno:longint; external name 'FPC_SYS_GETERRNO';
procedure fpseterrno(err:longint); external name 'FPC_SYS_SETERRNO';

{$endif}
end.

{
  $Log$
  Revision 1.8  2004-01-04 21:04:08  jonas
    * declare C-library routines as external in libc for Darwin (so we
      generate proper import entries)

  Revision 1.7  2003/12/31 20:01:00  marco
   * workaround for buggy 1.0 building

  Revision 1.6  2003/12/30 12:36:56  marco
   * FPC_USE_LIBC

  Revision 1.5  2003/12/11 18:20:50  florian
    * replaced VER1_0 by HASGLOBALPROPERTY

  Revision 1.4  2003/12/10 17:14:06  marco
   * property support under ifndef ver1_0

  Revision 1.3  2003/12/10 17:08:28  marco
   * property errno defined

  Revision 1.2  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.1  2002/12/18 16:44:09  marco
   * more new RTL

  Revision 1.2  2002/11/14 12:17:28  marco
   * for now.

}