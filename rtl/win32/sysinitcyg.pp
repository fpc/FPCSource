{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2006 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    Win32 cygwin startup code

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$asmmode att}
unit sysinitcyg;

  interface

  implementation

    procedure Cygwin_crt0(p : pointer);cdecl;external name 'cygwin_crt0';
    procedure __main;cdecl;external name '__main';

    procedure asm_exit;stdcall;public name 'asm_exit';
      begin
      end;

{$i sysinit.inc}


    procedure CMainEXE;cdecl;
      begin
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        __main;
        SetupEntryInformation;
{$ifdef FPC_USE_TLS_DIRECTORY}
        LinkIn(@tlsdir,@tls_callback_end,@tls_callback);
{$endif}
        EXE_Entry(SysInitEntryInformation);
      end;


    procedure CMainDLL;cdecl;
      begin
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        __main;
        SetupEntryInformation;
        DLL_Entry(SysInitEntryInformation);
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


    procedure _FPC_DLLMainCRTStartup(_hinstance : longint;_dllreason : dword;_dllparam:Pointer);stdcall;public name '_DLLMainCRTStartup';
      begin
        IsConsole:=true;
        sysinstance:=_hinstance;
        dllreason:=_dllreason;
        dllparam:=PtrInt(_dllparam);
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        Cygwin_crt0(@CMainDLL);
      end;


    procedure _FPC_DLLWinMainCRTStartup(_hinstance : longint;_dllreason : dword;_dllparam:Pointer);stdcall;public name '_DLLWinMainCRTStartup';
      begin
        IsConsole:=false;
        sysinstance:=_hinstance;
        dllreason:=_dllreason;
        dllparam:=PtrInt(_dllparam);
        asm
          subl   $0x8,%esp
          andl   $0xfffffff0,%esp
        end;
        Cygwin_crt0(@CMainDLL);
      end;

end.
