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
     TermInfo,
{$Ifdef ver1_0}
     linux;
{$else}
     termio;
{$endif}
{$endif UNIX}

  type
    TConsoleMode =
{$ifdef OS2}
      dword
{$endif OS2}
{$ifdef UNIX}
      TermIos
{$endif UNIX}
{$ifdef Win32}
      dword
{$endif Win32}
{$ifdef go32v2}
      longint
{$endif go32v2}
{$ifdef netware}
      longint
{$endif netware}
    ;

Procedure SaveConsoleMode(var ConsoleMode : TConsoleMode);
Procedure RestoreConsoleMode(const ConsoleMode : TConsoleMode);

implementation
{$ifdef Win32}
  uses
    windows;
{$endif Win32}

Procedure SaveConsoleMode(var ConsoleMode : TConsoleMode);
Begin
{$ifdef UNIX}
  TCGetAttr(1,ConsoleMode);
{$endif UNIX}
{$ifdef Win32}
  GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),ConsoleMode);
{$endif Win32}
{$ifdef go32v2}
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
{$ifdef Win32}
  SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE),ConsoleMode);
{$endif Win32}
{$ifdef go32v2}
{$endif go32v2}
End;

end.
