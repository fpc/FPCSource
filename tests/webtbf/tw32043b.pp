{ %cpu=avr }
{ %fail }
const
  cFE = $FE;
  c102 = $102;
  c1000 = $1000;
  c3070 = $3070;

procedure p;assembler;
var
  var1: byte;
  varabs: byte absolute $100;

asm
          adiw 0x1225 //(7)
          sts varabs, r22
          sts 0x1223, r22
end;

begin
end.
