{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Sven Barth, member of the Free Pascal development
    team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure PascalMain; external name 'PASCALMAIN';

{$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}
procedure SysEntry(constref info: TEntryInformation); external name 'FPC_SysEntry';

var
  InitFinalTable : record end; external name 'INITFINAL';
  ThreadvarTablesTable : record end; external name 'FPC_THREADVARTABLES';
  {$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
  WideInitTables : record end; external name 'FPC_WIDEINITTABLES';
  {$endif}
  {$ifdef FPC_HAS_RESSTRINITS}
  ResStrInitTables : record end; external name 'FPC_RESSTRINITTABLES';
  {$endif}
  ResourceStringTables : record end; external name 'FPC_RESOURCESTRINGTABLES';
  ResLocation : Pointer; external name 'FPC_RESLOCATION';

const
  SysInitEntryInformation : TEntryInformation = (
    InitFinalTable : @InitFinalTable;
    ThreadvarTablesTable : @ThreadvarTablesTable;
    ResourceStringTables : @ResourceStringTables;
{$ifdef FPC_HAS_RESSTRINITS}
    ResStrInitTables : @ResStrInitTables;
{$else}
    ResStrInitTables : nil;
{$endif}
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
    WideInitTables : @WideInitTables;
{$endif}
    ResLocation : @ResLocation;
    asm_exit : nil;
    PascalMain : @PascalMain;
    valgrind_used : false;
    Platform: (
        argc: 0;
        argv: nil;
        envp: nil;
        stkptr: nil;
        haltproc: nil;
      );
    );
{$endif}
