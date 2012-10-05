{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implement OS-dependent part of dynamic library loading.

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
  { using PtrInt here is compliant with the other platforms }
  TLibHandle = PtrInt;

Const
  NilHandle = TLibHandle(0);
// these are for easier crossplatform construction of dll names in dynloading libs.
{$ifdef Darwin}
  SharedSuffix = 'dylib';
{$else Darwin}
  SharedSuffix = 'so';
{$endif Darwin}

{$else}

{ ---------------------------------------------------------------------
    Implementation section
  ---------------------------------------------------------------------}

uses dl;

Function LoadLibrary(const Name : AnsiString) : TLibHandle;

begin
  Result:=TLibHandle(dlopen(Pchar(Name),RTLD_LAZY));
end;

Function GetProcedureAddress(Lib : TLibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=dlsym(lib,pchar(ProcName));
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=dlClose(Lib)=0;
end;

Function GetLoadErrorStr: string;

begin
  Result:=dl.dlerror;
end;

{$endif}

