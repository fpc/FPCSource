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


Function SafeLoadLibrary(Name : AnsiString) : TLibHandle;
Function LoadLibrary(Name : AnsiString) : TLibHandle;
Function GetProcedureAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;
Function UnloadLibrary(Lib : TLibHandle) : Boolean;

// Kylix/Delphi compability

Function FreeLibrary(Lib : TLibHandle) : Boolean;
Function GetProcAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;

Type
  HModule = TLibHandle; 

// these are for easier crossplatform construction of dll names in dynloading libs.
Const
 {$ifdef Windows}
  SharedSuffix  = 'dll';
 {$else}
   {$ifdef Darwin}
     SharedSuffix = 'dylib';
   {$else}
     {$ifdef OS2}
       SharedSuffix = 'dll';
     {$else}
       SharedSuffix = 'so';  
     {$endif}
   {$endif}
 {$endif}      
      
Implementation

{ ---------------------------------------------------------------------
  OS - Independent declarations.
  ---------------------------------------------------------------------}

{$i dynlibs.inc}

Function FreeLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=UnloadLibrary(lib);
end;

Function GetProcAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;

begin
  Result:=GetProcedureAddress(Lib,Procname);
end;

Function SafeLoadLibrary(Name : AnsiString) : TLibHandle;

{$ifdef i386}
 var w : word;
{$endif}


Begin
{$ifdef i386}
  w:=get8087cw;
{$endif}
 result:=loadlibrary(name);

{$ifdef i386}
  set8087cw(w);
{$endif}
End;

end.
