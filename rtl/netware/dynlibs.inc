{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2011 by the Free Pascal development team

    Implements OS dependent part for loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IFNDEF FPC_DOTTEDUNITS}
Uses nwserv;
{$ELSE}
Uses NetwareApi.nwserv;
{$ENDIF}

Function SysLoadLibraryA(const Name : RawByteString) : TlibHandle;
var args : array[0..1] of PAnsiChar;
begin
  args[0] := PAnsiChar(Name);
  args[1] := nil;
  Result:=spawnvp(P_NOWAIT,@args,nil);
end;

Function SysLoadLibraryU(const Name: UnicodeString) : TLibHandle;
begin
  Result := SysLoadLibraryA(ToSingleByteFileSystemEncodedFileName(Name));
end;

Function SysGetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=ImportSymbol(GetNlmHandle, PAnsiChar(ProcName));
end;

Function SysUnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=false;
end;


Function SysGetLoadErrorStr: AnsiString;

begin  
  Result:='';
end;


const
  SysDynLibsManager: TDynLibsManager = (
    LoadLibraryU: @SysLoadLibraryU;
    LoadLibraryA: @SysLoadLibraryA;
    GetProcAddress: @SysGetProcedureAddress;
    GetProcAddressOrdinal: Nil;
    UnloadLibrary: @SysUnloadLibrary;
    GetLoadErrorStr: @SysGetLoadErrorStr;
  );

procedure InitDynLibs;
begin
  SetDynLibsManager(SysDynLibsManager);
end;

