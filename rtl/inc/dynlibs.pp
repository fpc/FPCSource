{
    $Id: dynlibs.pp,v 1.7 2005/05/04 09:04:58 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implements OS-independent loading of dynamic libraries.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE OBJFPC}

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


Function LoadLibrary(Name : AnsiString) : TLibHandle;
Function GetProcedureAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;
Function UnloadLibrary(Lib : TLibHandle) : Boolean;

// Kylix/Delphi compability

Function FreeLibrary(Lib : TLibHandle) : Boolean;
Function GetProcAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;

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

Function GetProcAddress(Lib : TlibHandle; ProcName : AnsiString) : Pointer;

begin
  Result:=GetProcedureAddress(Lib,Procname);
end;

end.

{
  $Log: dynlibs.pp,v $
  Revision 1.7  2005/05/04 09:04:58  michael
  + Added HModule compatibility

  Revision 1.6  2005/02/14 17:13:22  peter
    * truncate log

}
