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

{$ifdef BSD}
Unit {$ifdef VER1_0}SysBSD{$else}System{$endif};
{$else}
Unit {$ifdef VER1_0}Syslinux{$else}System{$endif};
{$endif}

Interface

{$I sysunixh.inc}

Implementation

Var Errno : longint;

function geterrno:longint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 GetErrno:=Errno;
end;

procedure seterrno(err:longint); [public, alias: 'FPC_SYS_SETERRNO'];

begin
 Errno:=err;
end;

{ OS independant parts}

{$I system.inc}
{ OS dependant parts  }

{$I errno.inc}
{$I bunxtype.inc}
{$I ossysc.inc}
{$I osmain.inc}
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
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}  
End.

{
  $Log$
  Revision 1.8  2003-01-05 19:01:28  marco
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