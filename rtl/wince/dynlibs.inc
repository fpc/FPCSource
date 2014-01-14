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

Function DoLoadLibrary(const Name : UnicodeString) : TlibHandle;
begin
  Result:=Windows.LoadLibrary(PWideChar(Name));
end;

Function GetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;
var
  ws: PWideChar;
begin
  ws:=StringToPWideChar(ProcName);
  Result:=Windows.GetProcAddress(Lib, ws);
  FreeMem(ws);
end;

{$define DYNLIBS_SUPPORTS_ORDINAL}
Function GetProcedureAddress(Lib : TLibHandle; Ordinal : Word) : Pointer;
begin
  Result:=Windows.GetProcAddress(Lib, PWideChar(Ordinal));
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;
begin
  Result:=Windows.FreeLibrary(Lib);
end;

Function GetLoadErrorStr: string;

Var
  rc,c : integer;
  w : widestring;
  
begin  
  rc := GetLastError;
  SetLength(w,255);
  C:=FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,nil,rc,
                 MakeLangId(LANG_NEUTRAL, SUBLANG_DEFAULT),
                 @W[1], 255,nil);
  SetLength(w,c);
  Result:=w;
end;

{$endif}
