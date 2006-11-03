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
unit sysinitgprof;

  interface

  implementation

    {$linklib gmon}
    {$linklib gcc}
    {$linklib cygwin}
    {$linklib user32}
    {$linklib kernel32}

    var
      SysInstance : Longint;external name '_FPC_SysInstance';
      etext : record end;external name 'etext';
      monstarted : dword;

    procedure EXE_Entry; external name '_FPC_EXE_Entry';
    function DLL_Entry : longbool; external name '_FPC_DLL_Entry';

    procedure Cygwin_crt0(p : pointer);cdecl;external name 'cygwin_crt0';
    procedure __main;cdecl;external name '__main';

    procedure monstartup(main,etext : pointer);cdecl;external name 'monstartup';

    procedure CMainEXE;cdecl;forward;
    procedure CMainDLL;cdecl;forward;

    procedure EXEgmon_start;
      begin
        if monstarted=0 then
          begin
            inc(monstarted);
            monstartup(@CMainExe,@etext);
          end;
      end;


    procedure DLLgmon_start;
      begin
        if monstarted=0 then
          begin
            inc(monstarted);
            monstartup(@CMainDLL,@etext);
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
        EXE_Entry;
      end;


    procedure CMainDLL;cdecl;
      begin
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        DLLgmon_start;
        __main;
        DLL_Entry;
      end;


    procedure _FPC_mainCRTStartup;stdcall;public name '_mainCRTStartup';
      begin
        IsConsole:=true;
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
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

end.

