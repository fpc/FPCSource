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
  TLibHandle = Longint;

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
var
  ws: PWideChar;
begin
  ws:=StringToPWideChar(Name);
  Result:=Windows.LoadLibrary(ws);
  FreeMem(ws);
end;

Function GetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;
var
  ws: PWideChar;
begin
  ws:=StringToPWideChar(ProcName);
  Result:=Windows.GetProcAddress(Lib, ws);
  FreeMem(ws);
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;
begin
  Result:=Windows.FreeLibrary(Lib);
end;

Function GetLoadErrorStr: string;

Var
  rc,c : integer;
  
begin  
  rc := GetLastError;
  SetLength(Result,255);
  C:=FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM,nil,rc,
                 MakeLangId(LANG_NEUTRAL, SUBLANG_DEFAULT),
                 @Result[1], 255,nil);
  SetLength(Result,c);
end;

{$endif}
