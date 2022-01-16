{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020 by the Free Pascal development team.

    These files adds support for TP styled port accesses

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ to give easy port access like tp with port[] }

procedure tport.writeport(p : byte;data : byte);inline;
begin
  fpc_z80_outport(p,data);
end;


function tport.readport(p : byte) : byte;inline;
begin
  readport:=fpc_z80_inport(p);
end;

