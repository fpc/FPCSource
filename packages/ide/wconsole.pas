{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 2001 by Pierre Muller

    This unit is used to save and restore console modes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WConsole;

interface
{$ifdef UNIX}
   uses
     termio;
{$endif UNIX}

  type
    TConsoleMode =
{$ifdef OS2}
      dword
{$endif OS2}
{$ifdef UNIX}
      TermIos
{$endif UNIX}
{$ifdef Windows}
      dword
{$endif Windows}
{$ifdef go32v2}
      longint
{$endif go32v2}
{$ifdef netware}
      longint
{$endif netware}
{$ifdef amiga}
      longint
{$endif amiga}
{$ifdef morphos}
      longint
{$endif morphos}
{$ifdef aros}
      longint
{$endif aros}
    ;
Procedure SaveConsoleMode(var ConsoleMode : TConsoleMode);
Procedure RestoreConsoleMode(const ConsoleMode : TConsoleMode);

implementation
{$ifdef Windows}
  uses
    wutils,
    windows;
{$endif Windows}
{$ifdef GO32V2}
  uses
    Dpmiexcp;
{$endif GO32V2}

Procedure SaveConsoleMode(var ConsoleMode : TConsoleMode);
Begin
{$ifdef UNIX}
  TCGetAttr(1,ConsoleMode);
{$endif UNIX}
{$ifdef Windows}
  if not GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),ConsoleMode) then
    DebugMessage('','Call to GetConsoleMode failed, GetLastError='+
        IntToStr(GetLastError),0,0);
{$endif Windows}
{$ifdef go32v2}
  if djgpp_set_ctrl_c(false) then
    ConsoleMode:=1
  else
    ConsoleMode:=0;
{$endif go32v2}
{$ifdef netware}
  ConsoleMode:=0;
{$endif}
End;

Procedure RestoreConsoleMode(const ConsoleMode : TConsoleMode);
Begin
{$ifdef UNIX}
  TCSetAttr(1,TCSANOW,ConsoleMode);
{$endif UNIX}
{$ifdef Windows}
  if not SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),ConsoleMode) then
    DebugMessage('','Call to SetConsoleMode failed, GetLastError='+
        IntToStr(GetLastError),0,0);
{$endif Windows}
{$ifdef go32v2}
  djgpp_set_ctrl_c((ConsoleMode and 1)<>0);
{$endif go32v2}
End;

end.
