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

{$ifdef Darwin}
var argc:cardinal;
    argv:PPchar;
    envp:PPchar;
{$endif}

CONST SIGSTKSZ = 40960;

Implementation

{$I system.inc}

procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
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

function GetProcessID: SizeUInt;
begin
 GetProcessID := SizeUInt (fpGetPID);
end;


Begin
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackLength := InitialStkLen;
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
  { threading }
  InitSystemThreads;
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
{$ifdef HASWIDESTRING}
  initwidestringmanager;
{$endif HASWIDESTRING}
End.

{
  $Log$
  Revision 1.23  2005-02-06 12:16:52  peter
    * bsd thread updates

  Revision 1.22  2005/02/01 20:22:49  florian
    * improved widestring infrastructure manager

  Revision 1.21  2004/12/05 14:36:37  hajny
    + GetProcessID added

  Revision 1.20  2004/11/04 09:32:31  peter
  ErrOutput added

  Revision 1.19  2004/07/17 15:31:03  jonas
    * initialise StackLength (fixes stack checking in general, and tw2897 in
      particular)

  Revision 1.18  2004/07/03 22:49:34  daniel
    * Moved declarations downwards

  Revision 1.17  2004/07/03 22:44:37  daniel
    * Declared envp,argc,argv in interface for Darwin

  Revision 1.16  2004/06/19 08:06:04  marco
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
