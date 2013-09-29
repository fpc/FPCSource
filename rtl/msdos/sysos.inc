{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure GetInOutRes(def: Word);
var
  regs : Registers;
begin
  regs.AX:=$5900;
  regs.BX:=$0;
  MsDos(regs);
  InOutRes:=regs.AX;
  case InOutRes of
   19 : InOutRes:=150;
   21 : InOutRes:=152;
   32 : InOutRes:=5;
  end;
  if InOutRes=0 then
    InOutRes:=Def;
end;

