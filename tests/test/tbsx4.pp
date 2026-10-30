{ %OPT=-O- -O2 }

{ bsf/bsr do not allow a memory reference as destination, so the register allocator must
  not replace a spilled destination register of bsf/bsr by its spill location.

  The locals are used heavily in the loop so their weight is high, while the result of
  bsf/bsr is cheap to spill: it has only two uses (the "or 1" avoids the zero check) and
  it is live while all locals are still in use. }
program tbsx4;

{$mode objfpc}

type
{$ifdef cpu64}
  tval = qword;
{$else cpu64}
  tval = dword;
{$endif cpu64}

var
  g : array[0..15] of tval;
  gx : tval;

function testbsf(p,n : tval) : tval;
var
  i,l0,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15 : tval;
begin
  l0:=p*3; l1:=p*5; l2:=p*7; l3:=p*11; l4:=p*13; l5:=p*17; l6:=p*19; l7:=p*23;
  l8:=p*29; l9:=p*31; l10:=p*37; l11:=p*41; l12:=p*43; l13:=p*47; l14:=p*53; l15:=p*59;
  for i:=1 to n do
    begin
      l0:=l0+l1; l1:=l1+l2; l2:=l2+l3; l3:=l3+l4; l4:=l4+l5; l5:=l5+l6; l6:=l6+l7; l7:=l7+l8;
      l8:=l8+l9; l9:=l9+l10; l10:=l10+l11; l11:=l11+l12; l12:=l12+l13; l13:=l13+l14; l14:=l14+l15; l15:=l15+l0;
    end;
{$ifdef cpu64}
  result:=BsfQWord(gx or 1)+
{$else cpu64}
  result:=BsfDWord(gx or 1)+
{$endif cpu64}
    (l0 xor g[0])+(l1 xor g[1])+(l2 xor g[2])+(l3 xor g[3])+(l4 xor g[4])+(l5 xor g[5])+
    (l6 xor g[6])+(l7 xor g[7])+(l8 xor g[8])+(l9 xor g[9])+(l10 xor g[10])+(l11 xor g[11])+
    (l12 xor g[12])+(l13 xor g[13])+(l14 xor g[14])+(l15 xor g[15]);
end;

function testbsr(p,n : tval) : tval;
var
  i,l0,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15 : tval;
begin
  l0:=p*3; l1:=p*5; l2:=p*7; l3:=p*11; l4:=p*13; l5:=p*17; l6:=p*19; l7:=p*23;
  l8:=p*29; l9:=p*31; l10:=p*37; l11:=p*41; l12:=p*43; l13:=p*47; l14:=p*53; l15:=p*59;
  for i:=1 to n do
    begin
      l0:=l0+l1; l1:=l1+l2; l2:=l2+l3; l3:=l3+l4; l4:=l4+l5; l5:=l5+l6; l6:=l6+l7; l7:=l7+l8;
      l8:=l8+l9; l9:=l9+l10; l10:=l10+l11; l11:=l11+l12; l12:=l12+l13; l13:=l13+l14; l14:=l14+l15; l15:=l15+l0;
    end;
{$ifdef cpu64}
  result:=BsrQWord(gx or 1)+
{$else cpu64}
  result:=BsrDWord(gx or 1)+
{$endif cpu64}
    (l0 xor g[0])+(l1 xor g[1])+(l2 xor g[2])+(l3 xor g[3])+(l4 xor g[4])+(l5 xor g[5])+
    (l6 xor g[6])+(l7 xor g[7])+(l8 xor g[8])+(l9 xor g[9])+(l10 xor g[10])+(l11 xor g[11])+
    (l12 xor g[12])+(l13 xor g[13])+(l14 xor g[14])+(l15 xor g[15]);
end;

{ the same computation without bsf/bsr }
function reference(p,n : tval) : tval;
var
  i,l0,l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15 : tval;
begin
  l0:=p*3; l1:=p*5; l2:=p*7; l3:=p*11; l4:=p*13; l5:=p*17; l6:=p*19; l7:=p*23;
  l8:=p*29; l9:=p*31; l10:=p*37; l11:=p*41; l12:=p*43; l13:=p*47; l14:=p*53; l15:=p*59;
  for i:=1 to n do
    begin
      l0:=l0+l1; l1:=l1+l2; l2:=l2+l3; l3:=l3+l4; l4:=l4+l5; l5:=l5+l6; l6:=l6+l7; l7:=l7+l8;
      l8:=l8+l9; l9:=l9+l10; l10:=l10+l11; l11:=l11+l12; l12:=l12+l13; l13:=l13+l14; l14:=l14+l15; l15:=l15+l0;
    end;
  result:=(l0 xor g[0])+(l1 xor g[1])+(l2 xor g[2])+(l3 xor g[3])+(l4 xor g[4])+(l5 xor g[5])+
    (l6 xor g[6])+(l7 xor g[7])+(l8 xor g[8])+(l9 xor g[9])+(l10 xor g[10])+(l11 xor g[11])+
    (l12 xor g[12])+(l13 xor g[13])+(l14 xor g[14])+(l15 xor g[15]);
end;

var
  i : longint;
begin
  for i:=low(g) to high(g) do
    g[i]:=i*1234567;
  { bit 0 is always set by "or 1" in the tests, so bsf returns 0 }
  gx:=$100;
  if testbsf(1,10)<>reference(1,10) then
    halt(1);
  { bsr returns the index of the highest set bit }
  if testbsr(1,10)<>reference(1,10)+8 then
    halt(2);
  writeln('ok');
end.
