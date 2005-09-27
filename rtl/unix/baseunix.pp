{
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

  function fpgeterrno:longint; external name 'FPC_SYS_GETERRNO';
  procedure fpseterrno(err:longint); external name 'FPC_SYS_SETERRNO';
  property errno : cint read fpgeterrno write fpseterrno;

{$i bunxovlh.inc}


implementation

{$ifdef hassysctl}
Uses Sysctl;
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
