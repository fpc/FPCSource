{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2006 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    Win32 cygwin profiler startup code

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$asmmode att}
unit sysinitgprof;

  interface

  implementation

    const
      monstarted : dword = 0;

    var
      SysInstance : Longint;external name '_FPC_SysInstance';
      EntryInformation : TEntryInformation;

      InitFinalTable : record end; external name 'INITFINAL';
      ThreadvarTablesTable : record end; external name 'FPC_THREADVARTABLES';
      valgrind_used : boolean;external name '__fpc_valgrind';
      stext : record end;external name '__text_start__';
      etext : record end;external name 'etext';

    procedure EXE_Entry; external name '_FPC_EXE_Entry';
    function DLL_Entry : longbool; external name '_FPC_DLL_Entry';

    procedure Cygwin_crt0(p : pointer);cdecl;external name 'cygwin_crt0';
    procedure __main;cdecl;external name '__main';
    procedure _mcleanup;cdecl;external name '_mcleanup';

    procedure monstartup(main,etext : pointer);cdecl;external name 'monstartup';

    procedure CMainEXE;cdecl;forward;
    procedure CMainDLL;cdecl;forward;

    const
      STD_INPUT_HANDLE = dword(-10);

    function GetStdHandle(nStdHandle:DWORD) : THandle; stdcall; external 'kernel32' name 'GetStdHandle';
    function GetConsoleMode(hConsoleHandle: THandle; var lpMode: DWORD): Boolean; stdcall; external 'kernel32' name 'GetConsoleMode';

    procedure EXE_Entry(const info : TEntryInformation); external name '_FPC_EXE_Entry';
    function DLL_entry(const info : TEntryInformation) : longbool; external name '_FPC_DLL_Entry';
    procedure PascalMain;stdcall;external name 'PASCALMAIN';

    procedure asm_exit;stdcall;public name 'asm_exit';
      begin
        _mcleanup;
      end;


    procedure SetupEntryInformation;
      begin
        EntryInformation.InitFinalTable:=@InitFinalTable;
        EntryInformation.ThreadvarTablesTable:=@ThreadvarTablesTable;
        EntryInformation.asm_exit:=@asm_exit;
        EntryInformation.PascalMain:=@PascalMain;
        EntryInformation.valgrind_used:=valgrind_used;
      end;


    procedure EXEgmon_start;
      begin
        if monstarted=0 then
          begin
            inc(monstarted);
            monstartup(@stext,@etext);
          end;
      end;


    procedure DLLgmon_start;
      begin
        if monstarted=0 then
          begin
            inc(monstarted);
            monstartup(@stext,@etext);
          end;
      end;


    procedure CMainEXE;cdecl;
      begin
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        EXEgmon_start;
        __main;
        SetupEntryInformation;
        EXE_Entry(EntryInformation);
      end;


    procedure CMainDLL;cdecl;
      begin
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        DLLgmon_start;
        __main;
        SetupEntryInformation;
        DLL_Entry(EntryInformation);
      end;


    procedure _FPC_mainCRTStartup;stdcall;public name '_mainCRTStartup';
      begin
        IsConsole:=true;
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        { it seems cygwin messed around with the console mode so we've to
          store the startup console mode before cygwin can do anything (FK)
        }
        GetConsoleMode(GetStdHandle((Std_Input_Handle)),StartupConsoleMode);
        Cygwin_crt0(@CMainEXE);
      end;


    procedure _FPC_WinMainCRTStartup;stdcall;public name '_WinMainCRTStartup';
      begin
        IsConsole:=false;
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        Cygwin_crt0(@CMainEXE);
      end;


    procedure _FPC_DLLMainCRTStartup(_hinstance,_dllreason,_dllparam:longint);stdcall;public name '_DLLMainCRTStartup';
      begin
        IsConsole:=true;
        sysinstance:=_hinstance;
        dllreason:=_dllreason;
        dllparam:=_dllparam;
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        Cygwin_crt0(@CMainDLL);
      end;


    procedure _FPC_DLLWinMainCRTStartup(_hinstance,_dllreason,_dllparam:longint);stdcall;public name '_DLLWinMainCRTStartup';
      begin
        IsConsole:=false;
        sysinstance:=_hinstance;
        dllreason:=_dllreason;
        dllparam:=_dllparam;
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        Cygwin_crt0(@CMainDLL);
      end;

{$warnings off}
    {$linklib gmon}
    {$linklib gcc}
    {$linklib cygwin}
    {$linklib user32}
    {$linklib kernel32}

end.
