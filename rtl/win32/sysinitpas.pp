{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2006 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    Win32 pascal only startup code

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysinitpas;

  interface

  implementation


    procedure asm_exit;stdcall;public name 'asm_exit';
      begin
      end;

{$i sysinit.inc}

    procedure _FPC_mainCRTStartup;stdcall;public name '_mainCRTStartup';
    begin
      IsConsole:=true;
      { do it like it is necessary for the startup code linking against cygwin }
      GetConsoleMode(GetStdHandle((Std_Input_Handle)),StartupConsoleMode);
      SetupEntryInformation;
      Exe_entry(EntryInformation);
    end;


    procedure _FPC_WinMainCRTStartup;stdcall;public name '_WinMainCRTStartup';
    begin
      IsConsole:=false;
      SetupEntryInformation;
      Exe_entry(EntryInformation);
    end;


    procedure _FPC_DLLMainCRTStartup(_hinstance,_dllreason,_dllparam:longint);stdcall;public name '_DLLMainCRTStartup';
    begin
      IsConsole:=true;
      sysinstance:=_hinstance;
      dllreason:=_dllreason;
      dllparam:=_dllparam;
      SetupEntryInformation;
      DLL_Entry(EntryInformation);
    end;


    procedure _FPC_DLLWinMainCRTStartup(_hinstance,_dllreason,_dllparam:longint);stdcall;public name '_DLLWinMainCRTStartup';
    begin
      IsConsole:=false;
      sysinstance:=_hinstance;
      dllreason:=_dllreason;
      dllparam:=_dllparam;
      SetupEntryInformation;
      DLL_Entry(EntryInformation);
    end;

end.
