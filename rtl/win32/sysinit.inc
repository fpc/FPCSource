{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2006 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    Win32 startup code, shared part

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   var
      SysInstance : Longint;external name '_FPC_SysInstance';

      InitFinalTable : record end; external name 'INITFINAL';
      ThreadvarTablesTable : record end; external name 'FPC_THREADVARTABLES';
      valgrind_used : boolean;external name '__fpc_valgrind';

{$if defined(FPC_USE_TLS_DIRECTORY) or defined(FPC_SECTION_THREADVARS)}
   var
      tlsdir: record end; external name '__tls_used';

    procedure LinkIn(p1,p2,p3: Pointer); inline;
      begin
      end;
{$endif}

{$ifdef FPC_USE_TLS_DIRECTORY}
   var
      tls_callback_end: pointer; external name '__FPC_end_of_tls_callbacks';
      tls_callback: pointer; external name '__FPC_tls_callbacks';
{$endif FPC_USE_TLS_DIRECTORY}

    procedure EXE_Entry(const info : TEntryInformation); external name '_FPC_EXE_Entry';
    function DLL_Entry(const info : TEntryInformation) : longbool; external name '_FPC_DLL_Entry';
    procedure PascalMain;stdcall;external name 'PASCALMAIN';

    function GetStdHandle(nStdHandle:DWORD) : THandle; stdcall; external 'kernel32' name 'GetStdHandle';
    function GetConsoleMode(hConsoleHandle: THandle; var lpMode: DWORD): Boolean; stdcall; external 'kernel32' name 'GetConsoleMode';

    const
      STD_INPUT_HANDLE = dword(-10);
      SysInitEntryInformation : TEntryInformation = (
        InitFinalTable : @InitFinalTable;
        ThreadvarTablesTable : @ThreadvarTablesTable;
        asm_exit : @asm_exit;
        PascalMain : @PascalMain;
        valgrind_used : false;
        );


    procedure SetupEntryInformation;
      begin
        { valgind_used is the only thng that can change at startup
        EntryInformation.InitFinalTable:=@InitFinalTable;
        EntryInformation.ThreadvarTablesTable:=@ThreadvarTablesTable;
        EntryInformation.asm_exit:=@asm_exit;
        EntryInformation.PascalMain:=@PascalMain;}
        SysInitEntryInformation.valgrind_used:=valgrind_used;
      end;

{$define FPC_INSSIDE_SYSINIT}
{$include systlsdir.inc}

