{ %cpu=xtensa }
{ %norun }
unit asmtest;

interface

procedure test;

implementation

procedure test; assembler;
label
  lbl, lbl2;
asm
  bbci.l a4, 7, lbl2
  _bnez.n a4, lbl2
  loopnez a5, lbl
  beqz.n a2, lbl
  add a4, a5, a6
lbl:
  bt b9, lbl
  sub.s f1, f9, f13
  _loopgtz a3, lbl2
  rsr.prid a2
  extui a4, a3, 2, 1
lbl2:
  mula.aa.ll a3, a4
end;

end.

