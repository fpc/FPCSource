{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS dependent part for loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


Function SysLoadLibraryU(const Name : UnicodeString) : TlibHandle;

begin
  Result:=WinLoadLibraryW(PWideChar(Name));
end;

Function SysLoadLibraryA(const Name: RawByteString) : TLibHandle;

begin
  Result:=WinLoadLibraryW(PWideChar(UnicodeString(Name)));
end;

Function SysGetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=WinGetProcAddress(Lib,PChar(ProcName));
end;

{$push}
{$warn 4056 off}
Function SysGetProcedureAddressOrdinal(Lib : TLibHandle; Ordinal : TOrdinalEntry) : Pointer;

begin
  Result:=WinGetProcAddress(Lib,PChar(Ordinal));
end;
{$pop}

Function SysUnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=WinFreeLibrary(Lib);
end;

Function SysGetLoadErrorStr: string;

Var
  rc,c : integer;
  temp: WideString;
begin  
  rc := GetLastError;
  SetLength(temp,255);
  C:=FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM,nil,rc,
                 MakeLangId(LANG_NEUTRAL, SUBLANG_DEFAULT),
                 @temp[1], 255,nil);
  SetLength(temp,c);
  Result:=AnsiString(temp);
end;

const
  SysDynLibsManager: TDynLibsManager = (
    LoadLibraryU: @SysLoadLibraryU;
    LoadLibraryA: @SysLoadLibraryA;
    GetProcAddress: @SysGetProcedureAddress;
    GetProcAddressOrdinal: @SysGetProcedureAddressOrdinal;
    UnloadLibrary: @SysUnloadLibrary;
    GetLoadErrorStr: @SysGetLoadErrorStr;
  );

procedure InitSystemDynLibs;
begin
  SetDynLibsManager(SysDynLibsManager);
end;
