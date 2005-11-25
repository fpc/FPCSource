{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 Karoly Balogh

    abox.lib implementation for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit aboxlib;

interface

function DoMethod(obj : longword; msg : array of LongWord): longword;
function DoMethodA(obj : longword; msg1 : Pointer): longword; assembler;

implementation

function DoMethodA(obj : longword; msg1 : Pointer): longword; assembler;
asm
  mflr r31

  lwz r9,-4(r3)
  stw r9,32(r2)
  stw r4,36(r2)
  stw r3,40(r2)

  lwz r11,104(r2)
  lwz r3,8(r9)
  mtlr r11
  blrl

  mtlr r31
end ['R31'];

function DoMethod(obj : longword; msg : array of LongWord): longword;
begin
  DoMethod:=DoMethodA(obj, @msg);
end;

end.
