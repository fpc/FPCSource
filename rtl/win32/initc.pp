{
    This file is part of the Free Pascal run time library.
    This unit implements cygwin initialization
    Copyright (c) 1999-2006 by the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit initc;
{$ELSE FPC_DOTTEDUNITS}
unit initc;
{$ENDIF FPC_DOTTEDUNITS}

interface
{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes;
{$ENDIF FPC_DOTTEDUNITS}
  
function fpgetCerrno:cint;
procedure fpsetCerrno(err:cint);

property cerrno:cint read fpgetCerrno write fpsetcerrno;


implementation

function geterrnolocation: pcint; cdecl;external 'cygwin1.dll' name '__errno';

function fpgetCerrno:cint;
begin
  fpgetCerrno:=geterrnolocation^;
end;

procedure fpsetCerrno(err:cint);
begin
  geterrnolocation^:=err;
end;

end.
