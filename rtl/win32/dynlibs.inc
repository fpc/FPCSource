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

Uses windows;

Function LoadLibrary(Name : AnsiString) : TlibHandle;

begin
  Result:=Windows.LoadLibrary(PChar(Name));
end;

Function GetProcedureAddress(Lib : TLibHandle; ProcName : AnsiString) : Pointer;

begin
  Result:=Windows.GetProcAddress(Lib,PChar(ProcName));
end;

Function UnloadLibrary(Lib : TLibHandle) : Boolean;

begin
  Result:=Windows.FreeLibrary(Lib);
end;

{$endif}

{
  $Log$
  Revision 1.3  2004-06-12 13:30:33  michael
  Fixed bug 3156, as suggested by Michalis Kamburelis

  Revision 1.2  2002/09/07 16:01:28  peter
    * old logs removed and tabs fixed

}
