{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS-independent loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFDEF FPC}
{$MODE OBJFPC}
{$ENDIF}

unit dynlibs;

interface

Type
  TLibHandle = System.TLibHandle;

Const
  NilHandle = System.NilHandle;
  SharedSuffix = System.SharedSuffix;

Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle; inline;
Function LoadLibrary(const Name : RawByteString) : TLibHandle; inline;
Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle; inline;
Function LoadLibrary(const Name : UnicodeString) : TLibHandle; inline;

Function GetProcedureAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer; inline;
Function GetProcedureAddress(Lib : TLibHandle; Ordinal: TOrdinalEntry) : Pointer; inline;
Function UnloadLibrary(Lib : TLibHandle) : Boolean; inline;
Function GetLoadErrorStr: string; inline;

// Kylix/Delphi compability

Function FreeLibrary(Lib : TLibHandle) : Boolean; inline;
Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer; inline;

Type
  HModule = TLibHandle; 

Implementation


{ Should define a procedure InitDynLibs which sets up the DynLibs manager; optionally a
  DoneDynLibs can be defined which is called during finalization }
{$i dynlibs.inc}

Function SafeLoadLibrary(const Name : RawByteString) : TLibHandle;
begin
  Result:=System.SafeLoadLibrary(Name);
end;

Function LoadLibrary(const Name : RawByteString) : TLibHandle;
begin
  Result:=System.LoadLibrary(Name);
end;

Function SafeLoadLibrary(const Name : UnicodeString) : TLibHandle;
begin
  Result:=System.SafeLoadLibrary(Name);
end;

Function LoadLibrary(const Name : UnicodeString) : TLibHandle;
begin
  Result:=System.LoadLibrary(Name);
end;


Function GetProcedureAddress(Lib : TLibHandle; const ProcName: AnsiString) : Pointer;
begin
  Result:=System.GetProcedureAddress(Lib, ProcName);
end;

Function GetProcedureAddress(Lib : TLibHandle; Ordinal : TOrdinalEntry) : Pointer;
begin
  Result:=System.GetProcedureAddress(Lib, Ordinal);
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;
begin
  Result:=System.UnloadLibrary(Lib);
end;

Function GetLoadErrorStr: String;
begin
  Result:=System.GetLoadErrorStr;
end;

Function FreeLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=System.FreeLibrary(lib);
end;

Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=System.GetProcedureAddress(Lib,Procname);
end;

initialization
  InitDynLibs;
finalization
{$if declared(DoneDynLibs)}
  DoneDynLibs;
{$endif}
end.
