{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Solaris system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

interface

{$define FPC_IS_SYSTEM}

{ include system-independent routine headers }

{$I sysunixh.inc}

implementation

{ OS independant parts}

{$I system.inc}


procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;


function GetProcessID: SizeUInt;
begin
 GetProcessID := SizeUInt (fpGetPID);
end;


procedure pascalmain; external name 'PASCALMAIN';

{ Main entry point in C style, needed to capture program parameters. }
procedure main(argcparam: Longint; argvparam: ppchar; envpparam: ppchar); cdecl; [public];

begin
  argc:= argcparam;
  argv:= argvparam;
  envp:= envpparam;
  pascalmain;  {run the pascal main program}
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
 Revision 1.3  2005-02-13 21:47:56  peter
   * include file cleanup part 2

 Revision 1.2  2005/02/10 17:30:54  peter
   * renamed to solaris

 Revision 1.5  2005/02/07 22:17:26  peter
   * updated for 1.9.x unix rtl

 Revision 1.4  2005/02/01 20:22:50  florian
   * improved widestring infrastructure manager

 Revision 1.3  2004/12/05 14:36:38  hajny
   + GetProcessID added

 Revision 1.2  2004/11/06 22:22:28  florian
   * some sunos stuff from 1.0.x merged
}
