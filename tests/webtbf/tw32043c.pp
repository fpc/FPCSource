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
          adiw r28, c3070 //(6)
end;

begin
end.
