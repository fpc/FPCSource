{
    $Id$
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

{$else}

{ ---------------------------------------------------------------------
    Implementation section
  ---------------------------------------------------------------------}


Function LoadLibrary(Name : AnsiString) : TlibHandle;

begin
  Result:=NilHandle;
end;

Function GetProcedureAddress(Lib : TLibHandle; ProcName : AnsiString) : Pointer;

begin
  Result:=Nil;
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=False;
end;

{$endif}

{
  $Log$
  Revision 1.1  2000-08-18 19:15:34  michael
  + Implemented loading of dynamical libraries

}
