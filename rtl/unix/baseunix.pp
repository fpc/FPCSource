{
    $Id: baseunix.pp,v 1.4 2005/03/03 20:58:38 florian Exp $
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

{$i osdefs.inc}       { Compile time defines }

{$i aliasptp.inc}

{$packrecords C}

{$ifndef FPC_USE_LIBC}
  {$define FPC_USE_SYSCALL}
{$endif}

{$i errno.inc}          { Error numbers }
{$i ostypes.inc}

{$ifdef FPC_USE_LIBC}
  const clib = 'c';
  {$i oscdeclh.inc}
{$ELSE}
  {$i bunxh.inc}                { Functions}
{$ENDIF}

{$ifndef ver1_0}
  function fpgeterrno:longint; external name 'FPC_SYS_GETERRNO';
  procedure fpseterrno(err:longint); external name 'FPC_SYS_SETERRNO';
  property errno : cint read fpgeterrno write fpseterrno;
{$else}
  function fpgeterrno:longint;
  procedure fpseterrno(err:longint);
{$endif}

{$i bunxovlh.inc}


implementation

{$ifdef hassysctl}
Uses Sysctl;
{$endif}

{$ifdef ver1_0}
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

{$i genfuncs.inc}       // generic calls. (like getenv)
{$I gensigset.inc}     // general sigset funcs implementation.
{$I genfdset.inc}      // general fdset funcs.

{$ifndef FPC_USE_LIBC}
  {$i syscallh.inc}       // do_syscall declarations themselves
  {$i sysnr.inc}          // syscall numbers.
  {$i bsyscall.inc}  			// cpu specific syscalls
  {$i bunxsysc.inc}       // syscalls in system unit.
  {$i settimeo.inc}
{$endif}

{$i osmacro.inc}        { macro implenenations }
{$i bunxovl.inc}        { redefs and overloads implementation }

end.
{
  $Log: baseunix.pp,v $
  Revision 1.4  2005/03/03 20:58:38  florian
    + routines in baseunix can be overriden by processor specifics in bsyscall.inc

  Revision 1.3  2005/02/14 17:13:31  peter
    * truncate log

  Revision 1.2  2005/02/13 21:47:56  peter
    * include file cleanup part 2

  Revision 1.1  2005/02/13 20:01:38  peter
    * include file cleanup

}
