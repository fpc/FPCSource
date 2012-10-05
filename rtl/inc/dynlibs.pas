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

{ ---------------------------------------------------------------------
  Read OS-dependent interface declarations.
  ---------------------------------------------------------------------}

{$define readinterface}
{$i dynlibs.inc}
{$undef  readinterface}

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}


Function SafeLoadLibrary(const Name : AnsiString) : TLibHandle;
Function LoadLibrary(const Name : AnsiString) : TLibHandle;
Function GetProcedureAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;
Function UnloadLibrary(Lib : TLibHandle) : Boolean;
Function GetLoadErrorStr: string;

// Kylix/Delphi compability

Function FreeLibrary(Lib : TLibHandle) : Boolean;
Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;

Type
  HModule = TLibHandle; 

Implementation

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}

{$i dynlibs.inc}

Function FreeLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=UnloadLibrary(lib);
end;

Function GetProcAddress(Lib : TlibHandle; const ProcName : AnsiString) : Pointer;

begin
  Result:=GetProcedureAddress(Lib,Procname);
end;

Function SafeLoadLibrary(const Name : AnsiString) : TLibHandle;
{$if defined(cpui386) or defined(cpux86_64)}
  var
    fpucw : Word;
    ssecw : DWord;
{$endif}
  begin
    try
{$if defined(cpui386) or defined(cpux86_64)}
      fpucw:=Get8087CW;
{$ifdef cpui386}
      if has_sse_support then
{$endif cpui386}
        ssecw:=GetSSECSR;
{$endif}
{$if defined(windows) or defined(win32)}
      Result:=LoadLibraryA(PChar(Name));
{$else}
      Result:=loadlibrary(Name);
{$endif}
      finally
{$if defined(cpui386) or defined(cpux86_64)}
      Set8087CW(fpucw);
{$ifdef cpui386}
      if has_sse_support then
{$endif cpui386}
        SetSSECSR(ssecw);
{$endif}
    end;
  end;


end.
