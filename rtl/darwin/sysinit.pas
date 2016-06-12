{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements indirect entry point for executables and libaries

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sysinit;

interface

implementation

procedure PascalMain; external name '_PASCALMAIN';
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
  ResLocation: record end; external name 'FPC_RESLOCATION';
  ResourceStringTables : record end; external name 'FPC_RESOURCESTRINGTABLES';
  StkLen: SizeUInt; external name '__stklen';

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
    PascalMain : @PascalMain;
    valgrind_used : false;
    OS: (
        argc: 0;
        argv: nil;
        envp: nil;
        stklen: 0;
      );
    );


procedure FPC_SYSTEMMAIN(argcparam: Longint; argvparam: ppchar; envpparam: ppchar); cdecl; [public];
begin
  SysInitEntryInformation.OS.argc := argcparam;
  SysInitEntryInformation.OS.argv := argvparam;
  SysInitEntryInformation.OS.envp := envpparam;
  SysInitEntryInformation.OS.stklen := StkLen;
  SysEntry(SysInitEntryInformation);
end;

procedure FPC_LIBMAIN; cdecl; [public];
begin
  SysEntry(SysInitEntryInformation);
end;

end.
