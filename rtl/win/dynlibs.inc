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


{$ifdef readinterface}

{ ---------------------------------------------------------------------
    Interface declarations
  ---------------------------------------------------------------------}

Type
  TLibHandle = System.THandle;

Const
  NilHandle = 0;
// these are for easier crossplatform construction of dll names in dynloading libs.
  SharedSuffix = 'dll';

{$else}

{ ---------------------------------------------------------------------
    Implementation section
  ---------------------------------------------------------------------}

Uses windows;

Function LoadLibrary(const Name : AnsiString) : TlibHandle;

begin
  Result:=Windows.LoadLibrary(PChar(Name));
end;

Function GetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=Windows.GetProcAddress(Lib,PChar(ProcName));
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=Windows.FreeLibrary(Lib);
end;

Function GetLoadErrorStr: string;

Var
  rc : integer;
begin  
  rc := GetLastError;
  try
    result := Trim(SysErrorMessage(rc));
    if (result='') then
      result := 'Operating system error 0x' + IntToHex(rc, 8) + ' (no descriptive text)'
  except
    result := 'Operating system error 0x' + IntToHex(rc, 8) + ' (error getting descriptive text)'
  end;
end;

{$endif}

