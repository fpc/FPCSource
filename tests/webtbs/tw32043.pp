{ %cpu=avr }
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
          sts cFE, r22 //(1)
          sts c102, r22 //(2)
          sts c1000, r22 //(3)
          sts c3070, r22 //(4)
          sts var1, r22 //(5)
          sts 0x1223, r22
          sts varabs, r22
end;

begin
end.
