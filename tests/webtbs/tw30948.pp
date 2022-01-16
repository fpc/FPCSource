{$mode objfpc}
program BugInline;

uses
  sysutils;

{ Explicitly disable range and overflow checking }
{$R-}
{$Q-}

procedure Add64x2(HA,LA,HB,LB: QWord; out HR,LR: QWord); inline;
begin
  Inc(LA,LB);
  LR := LA;
  HR := HA + HB + Ord(LA < LB);
end;

procedure test;
  var XHA, XLA, XLB, HA1, LA1, LB1, HA2, LA2, LB2 : QWord;
begin
  XHA := QWord((Random($100000000) shl 32) or Random($100000000));
  XLA := QWord((Random($100000000) shl 32) or Random($100000000));
  XLB := QWord((Random($100000000) shl 32) or Random($100000000));

  // the bug appears only when the sum "XLA + XLB" produces a carry
  XLA := XLA or QWord($8000000000000000);
  XLB := XLB or QWord($8000000000000000);

  HA1 := XHA;
  LA1 := XLA;
  LB1 := XLB;
  Add64x2(HA1,LA1,0,LB1,HA1,LA1);
  Writeln('LA1:HA1 = $' + IntToHex(LA1,16) + ':$' + IntToHex(HA1,16)); // OK

  HA2 := XHA;
  LA2 := XLA;
  LB2 := XLB;
  Add64x2(HA2,LB2,0,LA2,HA2,LA2);
  Writeln('LA2:HA2 = $' + IntToHex(LA2,16) + ':$' + IntToHex(HA2,16)); // wrong

  if HA1 <> HA2 then
    begin
      Writeln('HA1 and HA2 are not equal!');
      halt(1);
    end;
end;

begin
  test;
end.

