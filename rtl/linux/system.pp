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
{$define oldreaddir}
{$define usedomain}
{$define posixworkaround}

{$I sysunixh.inc}

Implementation

{$ifdef ver1_0}
Var
{$else}
ThreadVar
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


{$I system.inc}

{ OS dependant parts  }

{$I errno.inc}                          // error numbers
{$I bunxtype.inc}                       // c-types, unix base types, unix
                                        //    base structures
{$I ossysc.inc}                         // base syscalls
{$I osmain.inc}                         // base wrappers *nix RTL (derivatives)

{ more OS independant parts}

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


procedure SysInitExecPath;
var
  hs   : string[16];
  link : string;
  i    : longint;
begin
  str(Fpgetpid,hs);
  hs:='/proc/'+hs+'/exe'#0;
  i:=Fpreadlink(@hs[1],@link[1],high(link));
  { it must also be an absolute filename, linux 2.0 points to a memory
    location so this will skip that }
  if (i>0) and (link[1]='/') then
   begin
     link[0]:=chr(i);
     ExecPathStr:=link;
   end;
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
{$ifdef HASVARIANT}
  initvariantmanager;
{$endif HASVARIANT}
End.

{
  $Log$
  Revision 1.8  2003-09-14 20:15:01  marco
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
