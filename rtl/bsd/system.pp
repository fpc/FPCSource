{
    $Id$
    This file is part of the Free Pascal run time librar~y.
    Copyright (c) 2000 by Marco van de Voort
    member of the Free Pascal development team.

    System unit for the *BSD's.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ These things are set in the makefile, }
{ But you can override them here.}

{ If you use an aout system, set the conditional AOUT}
{ $Define AOUT}

Unit {$ifdef VER1_0}SysBSD{$else}System{$endif};

Interface


{$define FPC_USE_SIGPROCMASK}
{$define FPC_USE_SIGALTSTACK}

{$ifndef FPC_USE_LIBC}
{$define FPC_USE_SYSCALL}
{$endif}

{$define FPC_IS_SYSTEM}

{$I sysunixh.inc}

CONST SIGSTKSZ = 40960;

Implementation

{$ifdef FPC_USE_LIBC}

const clib = 'c';

type libcint=longint;
     plibcint=^libcint;

{$ifdef FreeBSD} // tested on x86
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef NetBSD} // from a sparc dump.
function geterrnolocation: Plibcint; cdecl;external clib name '__errno';
{$else} 
{$ifdef Darwin}
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef OpenBSD}

var libcerrno : libcint; cvar;

function geterrnolocation: Plibcint; cdecl;

begin
 geterrnolocation:=@libcerrno;
end;

{$else}
{$endif}
{$endif}
{$endif}
{$endif}

function geterrno:libcint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 geterrno:=geterrnolocation^;
end;

procedure seterrno(err:libcint); [public, alias: 'FPC_SYS_SETERRNO'];
begin
  geterrnolocation^:=err;
end;

{$else}
{$ifdef ver1_0}
Var
{$else}
threadvar
{$endif}
      Errno : longint;

function geterrno:longint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 GetErrno:=Errno;
end;

procedure seterrno(err:longint); [public, alias: 'FPC_SYS_SETERRNO'];

begin
 Errno:=err;
end;
{$endif}

{ OS independant parts}

{$I system.inc}

{*****************************************************************************
      OS Memory allocation / deallocation 
 ****************************************************************************}

{ OS dependant parts  }

{$I errno.inc}
{$I bunxtype.inc}
{$I ossysc.inc}
{$I osmain.inc}

function SysOSAlloc(size: ptrint): pointer;
begin
  result := sbrk(size);
end;

{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptrint);
begin
  fpmunmap(p, size);
end;


{$I text.inc}
{$I heap.inc}


{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}


{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;


{$ifdef FPC_USE_LIBC}

{ can also be used with other BSD's if they use the system's crtX instead of prtX }

{$ifdef Darwin}
procedure pascalmain; external name 'PASCALMAIN';

{ Main entry point in C style, needed to capture program parameters. }
procedure main(argcparam: Longint; argvparam: ppchar; envpparam: ppchar); cdecl; [public];

begin
  argc:= argcparam;
  argv:= argvparam;
  envp:= envpparam;
  pascalmain;  {run the pascal main program}
end;
{$endif Darwin}
{$endif FPC_USE_LIBC}



Begin
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackBottom := Sptr - StackLength;
{ Set up signals handlers }
  InstallSignals;
{ Setup heap }
  InitHeap;
  SysInitExceptions;
{ Arguments }
  SetupCmdLine;
{ Setup stdin, stdout and stderr }
  SysInitStdIO;
{ Reset IO Error }
  InOutRes:=0;
(* This should be changed to a real value during *)
(* thread driver initialization if appropriate.  *)
  ThreadID := 1;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}  
End.

{
  $Log$
  Revision 1.16  2004-06-19 08:06:04  marco
   * moved heap.inc and text.inc before sysalloc as suggested. Why wasn't this
  	done directly?

  Revision 1.15  2004/06/17 16:16:13  peter
    * New heapmanager that releases memory back to the OS, donated
      by Micha Nelissen

  Revision 1.14  2004/01/22 13:46:14  marco
  bsd

  Revision 1.13  2004/01/20 23:09:14  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.12  2004/01/04 20:32:05  jonas
    + geterrnolocation for Darwin
    + C-style main for Darwin (generic, can be used for anything)

  Revision 1.11  2003/12/30 12:26:21  marco
   * FPC_USE_LIBC

  Revision 1.10  2003/10/26 17:01:04  marco
   * moved sigprocmask to system

  Revision 1.9  2003/10/26 16:42:22  marco
   * texception4 fix merge

  Revision 1.8  2003/01/05 19:01:28  marco
   * FreeBSD compiles now with baseunix mods.

  Revision 1.7  2002/11/12 14:57:48  marco
   * Ugly hack to temporarily be able to use system.pp for Linux too

  Revision 1.6  2002/10/27 11:58:30  marco
   * Modifications from Saturday.

  Revision 1.5  2002/10/26 18:27:51  marco
   * First series POSIX calls commits. Including getcwd.

  Revision 1.4  2002/10/18 12:19:58  marco
   * Fixes to get the generic *BSD RTL compiling again + fixes for thread
     support. Still problems left in fexpand. (inoutres?) Therefore fixed
     sysposix not yet commited

  Revision 1.3  2002/10/13 09:25:39  florian
    + call to initvariantmanager inserted

  Revision 1.2  2002/09/07 16:01:17  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/19 12:29:11  marco
   * First working POSIX *BSD system unit.

}
