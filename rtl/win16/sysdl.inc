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


var
  LastLoadLibraryError: 0..HINSTANCE_ERROR;


function SysLoadLibraryU(const Name : UnicodeString) : TlibHandle;
begin
  Result:=WinLoadLibrary(LPCSTR(AnsiString(Name)));
  if Result<=HINSTANCE_ERROR then
  begin
    LastLoadLibraryError:=Result;
    Result:=0;
  end;
end;

function SysLoadLibraryA(const Name: RawByteString) : TLibHandle;
begin
  Result:=WinLoadLibrary(LPCSTR(Name));
  if Result<=HINSTANCE_ERROR then
  begin
    LastLoadLibraryError:=Result;
    Result:=0;
  end;
end;

function SysGetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : FarPointer;
begin
  Result:=WinGetProcAddress(Lib,LPCSTR(ProcName));
end;

{$push}
{$warn 4056 off}
function SysGetProcedureAddressOrdinal(Lib : TLibHandle; Ordinal : TOrdinalEntry) : FarPointer;
begin
  Result:=WinGetProcAddress(Lib,LPCSTR(Ordinal));
end;
{$pop}

function SysUnloadLibrary(Lib : TLibHandle) : Boolean;
begin
  WinFreeLibrary(Lib);
  Result:=true;
end;

function SysGetLoadErrorStr: string;
var
  rc,c : integer;
  temp: WideString;
begin
  case LastLoadLibraryError of
    0: Result := 'System out of memory, executable file is corrupt, or contains invalid relocations';
    2: Result := 'File not found';
    3: Result := 'Path not found';
    5: Result := 'Attempt was made to dynamically link to a task, or there was a sharing or network-protection error';
    6: Result := 'Library requires separate data segments for each task';
    8: Result := 'Insufficient memory to start the application';
    10: Result := 'Incorrect Windows version';
    11: Result := 'Invalid executable file. Either it is not a Windows application or there is an error in the .EXE image';
    12: Result := 'Application is designed for a different operating system';
    13: Result := 'Application is designed for MS-DOS 4.0';
    14: Result := 'Type of executable is unknown';
    15: Result := 'Cannot load a real-mode application (developed for an earlier version of Windows)';
    16: Result := 'Cannot load a second instance of an executable file containing multiple data segments that are not marked read-only';
    19: Result := 'Cannot load a compressed executable file. The file must be decompressed before it can be loaded';
    20: Result := 'One of the DLLs required to run this application is corrupt';
    21: Result := 'Application requires Microsoft Windows 32-bit extensions';
    else
      Result := 'Unknown error code';
  end;
  WriteStr(Result, '(', LastLoadLibraryError, ') - ', Result);
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
