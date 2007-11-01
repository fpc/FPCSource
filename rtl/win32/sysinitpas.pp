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

    var
      SysInstance : Longint;external name '_FPC_SysInstance';
      EntryInformation : TEntryInformation;

      InitFinalTable : record end; external name 'INITFINAL';
      ThreadvarTablesTable : record end; external name 'FPC_THREADVARTABLES';
      valgrind_used : boolean;external name '__fpc_valgrind';

    procedure asm_exit;stdcall;public name 'asm_exit';
      begin
      end;

    procedure EXE_Entry(const info : TEntryInformation); external name '_FPC_EXE_Entry';
    function DLL_entry(const info : TEntryInformation) : longbool; external name '_FPC_DLL_Entry';
    procedure PascalMain;stdcall;external name 'PASCALMAIN';

    procedure SetupEntryInformation;
      begin
        EntryInformation.InitFinalTable:=@InitFinalTable;
        EntryInformation.ThreadvarTablesTable:=@ThreadvarTablesTable;
        EntryInformation.asm_exit:=@asm_exit;
        EntryInformation.PascalMain:=@PascalMain;
        EntryInformation.valgrind_used:=valgrind_used;
      end;

    const
      STD_INPUT_HANDLE = dword(-10);

    function GetStdHandle(nStdHandle:DWORD) : THandle; stdcall; external 'kernel32' name 'GetStdHandle';
    function GetConsoleMode(hConsoleHandle: THandle; var lpMode: DWORD): Boolean; stdcall; external 'kernel32' name 'GetConsoleMode';

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
