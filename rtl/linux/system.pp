{
    $Id$
    This file is part of the Free Pascal run time librar~y.
    Copyright (c) 2000 by Marco van de Voort
    member of the Free Pascal development team.

    System unit for Linux.

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

Unit {$ifdef VER1_0}Syslinux{$else}System{$endif};

Interface
{$define oldreaddir}
{$define usedomain}
{$define posixworkaround}
{$define FPC_IS_SYSTEM}
{$ifdef FPC_USE_LIBC}
{$define usegetcwd}
{$endif}

{$I sysunixh.inc}

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


procedure SysInitExecPath;
var
  i    : longint;
begin
  execpathstr[0]:=#0;
  i:=Fpreadlink('/proc/self/exe',@execpathstr[1],high(execpathstr));
  { it must also be an absolute filename, linux 2.0 points to a memory
    location so this will skip that }
  if (i>0) and (execpathstr[1]='/') then
     execpathstr[0]:=char(i);
end;

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
  SysInitExecPath;
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
  Revision 1.22  2005-02-06 11:20:52  peter
    * threading in system unit
    * removed systhrds unit

  Revision 1.21  2005/02/01 20:22:49  florian
    * improved widestring infrastructure manager

  Revision 1.20  2004/12/05 14:36:37  hajny
    + GetProcessID added

  Revision 1.19  2004/11/04 09:32:31  peter
  ErrOutput added

  Revision 1.18  2004/07/09 22:31:22  peter
    * fixed execpathstr setting

  Revision 1.17  2004/07/08 21:22:15  daniel
    * Tweaking...

  Revision 1.16  2004/07/08 19:45:42  daniel
    * Use /proc/self/exe instead of /proc/[getpid]/exe

  Revision 1.15  2004/06/17 16:16:13  peter
    * New heapmanager that releases memory back to the OS, donated
      by Micha Nelissen

  Revision 1.14  2004/01/20 23:09:14  hajny
    * ExecuteProcess fixes, ProcessID and ThreadID added

  Revision 1.13  2004/01/01 14:16:55  marco
   * getcwd missed cdecl

  Revision 1.12  2003/12/31 20:20:57  marco
   * small additions for getcwd what we need for FPC_USE_LIBC

  Revision 1.11  2003/12/30 16:26:10  marco
   * some more fixes. Testing on idefix

  Revision 1.10  2003/12/30 15:43:20  marco
   * linux now compiles with FPC_USE_LIBC

  Revision 1.9  2003/12/30 12:36:56  marco
   * FPC_USE_LIBC

  Revision 1.8  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.7  2003/04/30 22:11:06  florian
    + for a lot of x86-64 dependend files mostly dummies added

  Revision 1.6  2002/12/27 18:36:16  peter
    * Setup ExecPathStr for ParamStr(0)

  Revision 1.5  2002/12/18 20:42:29  peter
    * initial stacklen setup

  Revision 1.3  2002/12/18 16:44:09  marco
   * more new RTL

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
