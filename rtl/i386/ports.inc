{
    This file is part of the Free Pascal run time library.
    and implements some stuff for protected mode programming
    Copyright (c) 1999-2000 by the Free Pascal development team.

    These files adds support for TP styled port accesses

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$asmmode ATT}

{ to give easy port access like tp with port[] }

procedure tport.writeport(p : word;data : byte);inline;
begin
  fpc_x86_outportb(p,data);
end;


function tport.readport(p : word) : byte;inline;
begin
  readport:=fpc_x86_inportb(p);
end;


procedure tportw.writeport(p : word;data : word);inline;
begin
  fpc_x86_outportw(p,data);
end;


function tportw.readport(p : word) : word;inline;
begin
  readport:=fpc_x86_inportw(p);
end;


procedure tportl.writeport(p : word;data : longint);inline;
begin
  fpc_x86_outportl(p,data);
end;


function tportl.readport(p : word) : longint;inline;
begin
  readport:=fpc_x86_inportl(p);
end;
