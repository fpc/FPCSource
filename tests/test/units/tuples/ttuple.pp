program testtuples;

{$mode objfpc}
{$h+}

uses tuples;

Const
  Val1  = 'Value 1';
  Val2  = 42;
  Val3  = Char('A');
  Val4  = 3.14;
  Val5  = True;

Function TestPairAssign : String;

Var
  P : specialize TPair<String, Integer>;

begin
  P:=specialize Pair<String, Integer>(Val1, Val2);
  if (P.First<>Val1) or (P.Second<>Val2) then
    Exit('Pair construction failure');
  P:=specialize Tuple<String, Integer>(Val1, Val2);
  if (P.First<>Val1) or (P.Second<>Val2) then
    Exit('Pair construction failure');
  P:=specialize TPair<String, Integer>.Create(Val1, Val2);
  if (P.First<>Val1) or (P.Second<>Val2) then
    Exit('Pair construction failure');
end;

Function TestPairUnpack : String;

Var
  P : specialize TPair<String, Integer>;

  A: String;
  B: Integer;

begin
  P:=specialize Pair<String, Integer>(Val1, Val2);
  A:='';
  B:=0;
  P.Unpack(A, _);
  if A<>Val1 then
    Exit('Pair unpack failure');
  A:='';
  P.Unpack(_, B);
  if B<>Val2 then
    Exit('Pair unpack failure');
  B:=0;
  P.unpack(A, B);
  if (A<>Val1) or (B<>Val2) then
    Exit('Pair unpack failure');
end;

Function TestTripleAssign : String;

Var
  P : specialize TTriple<String, Integer, Char>;

begin
  P:=specialize Triple<String, Integer, Char>(Val1, Val2, Val3);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3) then
    Exit('Triple construction failure');
  P:=specialize Tuple<String, Integer, Char>(Val1, Val2, Val3);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3)  then
    Exit('Triple construction failure');
  P:=specialize TTriple<String, Integer,Char>.Create(Val1, Val2, Val3);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3)  then
    Exit('Triple construction failure');
end;

Function TestTripleUnpack : String;

Var
  P : specialize TTriple<String, Integer, Char>;

  A: String;
  B: Integer;
  C: Char;

begin
  P:=specialize Triple<String, Integer, Char>(Val1, Val2, Val3);
  A:='';
  B:=0;
  C:=#00;
  P.Unpack(A, _, _);
  if A<>Val1 then
    Exit('Triple unpack failure');
  A:='';
  P.Unpack(_, B, _);
  if B<>Val2 then
    Exit('Triple unpack failure');
  B:=0;
  P.unpack(_, _, C);
  if C<>Val3 then
    Exit('Triple unpack failure');
  C:=#00;
  P.unpack(A, B, _);
  if (A<>Val1) or (B<>Val2) then
    Exit('Triple unpack failure');
  A:='';
  B:=0;
  P.unpack(A, _, C);
  if (A<>Val1) or (C<>Val3) then
    Exit('Triple unpack failure');
  A:='';
  C:=#00;
  P.unpack(_, B, C);
  if (B<>Val2) or (C<>Val3) then
    Exit('Triple unpack failure');
  B:=0;
  C:=#00;
  P.unpack(A, B, C);
  if (A<>Val1) or (B<>Val2) or (C<>Val3)then
    Exit('Triple unpack failure');
end;

Function TestQuadrupleAssign : String;

Var
  P : specialize TQuadruple<String, Integer, Char, Extended>;

begin
  P:=specialize Quadruple<String, Integer, Char, Extended>(Val1, Val2, Val3, Val4);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3) or (P.Fourth<>Val4) then
    Exit('Quadruple construction failure');
  P:=specialize Tuple<String, Integer, Char, Extended>(Val1, Val2, Val3, Val4);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3) or (P.Fourth<>Val4)  then
    Exit('Quadruple construction failure');
  P:=specialize TQuadruple<String, Integer,Char, Extended>.Create(Val1, Val2, Val3, Val4);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3) or (P.Fourth<>Val4)  then
    Exit('Quadruple construction failure');
end;

Function TestQuadrupleUnpack : String;

Var
  P : specialize TQuadruple<String, Integer, Char, Extended>;

  A: String;
  B: Integer;
  C: Char;
  D: Extended;

begin
  P:=specialize Quadruple<String, Integer, Char, Extended>(Val1, Val2, Val3, Val4);
  A:='';
  B:=0;
  C:=#00;
  D:=0.0;
  P.Unpack(A, _, _, _);
  if A<>Val1 then
    Exit('Quadruple unpack failure');
  A:='';
  P.Unpack(_, B, _, _);
  if B<>Val2 then
    Exit('Quadruple unpack failure');
  B:=0;
  P.unpack(_, _, C, _);
  if C<>Val3 then
    Exit('Quadruple unpack failure');
  C:=#00;
  P.unpack(_, _, _, D);
  if D<>Val4 then
    Exit('Quadruple unpack failure');
  D:=0.0;
  P.unpack(A, B, _, _);
  if (A<>Val1) or (B<>Val2) then
    Exit('Quadruple unpack failure');
  A:='';
  B:=0;
  P.unpack(A, _, C, _);
  if (A<>Val1) or (C<>Val3) then
    Exit('Quadruple unpack failure');
  A:='';
  C:=#00;
  P.unpack(A, _, _, D);
  if (A<>Val1) or (D<>Val4) then
    Exit('Quadruple unpack failure');
  A:='';
  D:=0.0;
  P.unpack(_, B, C, _);
  if (B<>Val2) or (C<>Val3) then
    Exit('Quadruple unpack failure');
  B:=0;
  C:=#00;
  P.unpack(_, B, _, D);
  if (B<>Val2) or (D<>Val4) then
    Exit('Quadruple unpack failure');
  B:=0;
  D:=0.0;
  P.unpack(_, _, C, D);
  if (C<>Val3) or (D<>Val4) then
    Exit('Quadruple unpack failure');
  C:=#00;
  D:=0.0;
  P.unpack(A, B, C, _);
  if (A<>Val1) or (B<>Val2) or (C<>Val3)then
    Exit('Quadruple unpack failure');
  A:='';
  B:=0;
  C:=#00;
  P.unpack(A, B, _, D);
  if (A<>Val1) or (B<>Val2) or (D<>Val4)then
    Exit('Quadruple unpack failure');
  A:='';
  B:=0;
  D:=0.0;
  P.unpack(A, _, C, D);
  if (A<>Val1) or (C<>Val3) or (D<>Val4)then
    Exit('Quadruple unpack failure');
  A:='';
  C:=#00;
  D:=0.0;
  P.unpack(_, B, C, D);
  if (B<>Val2) or (C<>Val3) or (D<>Val4)then
    Exit('Quadruple unpack failure');
  B:=0;
  C:=#00;
  D:=0.0;
  P.unpack(A, B, C, D);
  if (A<>Val1) or (B<>Val2) or (C<>Val3) or (D<>Val4) then
    Exit('Quadruple unpack failure');
end;

Function TestQuintupleAssign : String;

Var
  P : specialize TQuintuple<String, Integer, Char, Extended, Boolean>;

begin
  P:=specialize Quintuple<String, Integer, Char, Extended, Boolean>(Val1, Val2, Val3, Val4, Val5);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3) or (P.Fourth<>Val4) or (P.Fifth<>Val5) then
    Exit('Quintuple construction failure');
  P:=specialize Tuple<String, Integer, Char, Extended, Boolean>(Val1, Val2, Val3, Val4, Val5);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3) or (P.Fourth<>Val4) or (P.Fifth<>Val5)  then
    Exit('Quintuple construction failure');
  P:=specialize TQuintuple<String, Integer,Char, Extended, Boolean>.Create(Val1, Val2, Val3, Val4, Val5);
  if (P.First<>Val1) or (P.Second<>Val2) or (P.Third<>Val3) or (P.Fourth<>Val4) or (P.Fifth<>Val5)  then
    Exit('Quintuple construction failure');
end;

Function TestQuintupleUnpack : String;

Var
  P : specialize TQuintuple<String, Integer, Char, Extended, Boolean>;

  A: String;
  B: Integer;
  C: Char;
  D: Extended;
  E: Boolean;

begin
  P:=specialize Quintuple<String, Integer, Char, Extended, Boolean>(Val1, Val2, Val3, Val4, Val5);
  A:='';
  B:=0;
  C:=#00;
  D:=0.0;
  E:=False;
  P.Unpack(A, _, _, _, _);
  if A<>Val1 then
    Exit('Quintuple unpack failure');
  A:='';
  P.Unpack(_, B, _, _, _);
  if B<>Val2 then
    Exit('Quintuple unpack failure');
  B:=0;
  P.unpack(_, _, C, _, _);
  if C<>Val3 then
    Exit('Quintuple unpack failure');
  C:=#00;
  P.unpack(_, _, _, D, _);
  if D<>Val4 then
    Exit('Quintuple unpack failure');
  D:=0.0;
  P.unpack(_, _, _, _, E);
  if E<>Val5 then
    Exit('Quintuple unpack failure');
  E:=False;
  P.unpack(A, B, _, _, _);
  if (A<>Val1) or (B<>Val2) then
    Exit('Quintuple unpack failure');
  A:='';
  B:=0;
  P.unpack(A, _, C, _, _);
  if (A<>Val1) or (C<>Val3) then
    Exit('Quintuple unpack failure');
  A:='';
  C:=#00;
  P.unpack(A, _, _, D, _);
  if (A<>Val1) or (D<>Val4) then
    Exit('Quintuple unpack failure');
  A:='';
  D:=0.0;
  P.unpack(A, _, _, _, E);
  if (A<>Val1) or (E<>Val5) then
    Exit('Quintuple unpack failure');
  A:='';
  E:=False;
  P.unpack(_, B, C, _, _);
  if (B<>Val2) or (C<>Val3) then
    Exit('Quintuple unpack failure');
  B:=0;
  C:=#00;
  P.unpack(_, B, _, D, _);
  if (B<>Val2) or (D<>Val4) then
    Exit('Quintuple unpack failure');
  B:=0;
  D:=0.0;
  P.unpack(_, B, _, _, E);
  if (B<>Val2) or (E<>Val5) then
    Exit('Quintuple unpack failure');
  B:=0;
  E:=False;
  P.unpack(_, _, C, D, _);
  if (C<>Val3) or (D<>Val4) then
    Exit('Quintuple unpack failure');
  C:=#00;
  D:=0.0;
  P.unpack(_, _, C, _, E);
  if (C<>Val3) or (E<>Val5) then
    Exit('Quintuple unpack failure');
  C:=#00;
  E:=False;
  P.unpack(_, _, _, D, E);
  if (D<>Val4) or (E<>Val5) then
    Exit('Quintuple unpack failure');
  D:=0.0;
  E:=False;
  P.unpack(A, B, C, _, _);
  if (A<>Val1) or (B<>Val2) or (C<>Val3)then
    Exit('Quintuple unpack failure');
  A:='';
  B:=0;
  C:=#00;
  P.unpack(A, B, _, D, _);
  if (A<>Val1) or (B<>Val2) or (D<>Val4)then
    Exit('Quintuple unpack failure');
  A:='';
  B:=0;
  D:=0.0;
  P.unpack(A, B, _, _, E);
  if (A<>Val1) or (B<>Val2) or (E<>Val5)then
    Exit('Quintuple unpack failure');
  A:='';
  B:=0;
  E:=False;
  P.unpack(_, B, C, D, _);
  if (B<>Val2) or (C<>Val3) or (D<>Val4)then
    Exit('Quintuple unpack failure');
  B:=0;
  C:=#00;
  D:=0.0;
  P.unpack(_, B, C, _, E);
  if (B<>Val2) or (C<>Val3) or (E<>Val5)then
    Exit('Quintuple unpack failure');
  B:=0;
  C:=#00;
  E:=False;
  P.unpack(_, _, C, D, E);
  if (C<>Val3) or (D<>Val4) or (E<>Val5)then
    Exit('Quintuple unpack failure');
  C:=#00;
  D:=0.0;
  E:=False;
  P.unpack(A, B, C, D, _);
  if (A<>Val1) or (B<>Val2) or (C<>Val3) or (D<>Val4) then
    Exit('Quintuple unpack failure');
  A:='';
  B:=0;
  C:=#00;
  D:=0.0;
  P.unpack(A, B, C, _, E);
  if (A<>Val1) or (B<>Val2) or (C<>Val3) or (E<>Val5) then
    Exit('Quintuple unpack failure');
  A:='';
  B:=0;
  C:=#00;
  E:=False;
  P.unpack(A, B, C, D, E);
  if (A<>Val1) or (B<>Val2) or (C<>Val3) or (D<>Val4) or (E<>Val5) then
    Exit('Quintuple unpack failure');
end;

Procedure DoTest(aTest,aResult : String);

begin
  if aResult<>'' then
    begin
    writeln(aTest,' failed : ',aResult);
    Halt(1);
    end
  else
    Writeln(aTest,' OK.');
end;


begin
  DoTest('TestPairAssign',TestPairAssign);
  DoTest('TestPairUnpack',TestPairUnpack);
  DoTest('TestTripleAssign',TestTripleAssign);
  DoTest('TestTripleUnpack',TestTripleUnpack);
  DoTest('TestQuadrupleAssign',TestQuadrupleAssign);
  DoTest('TestQuadrupleUnpack',TestQuadrupleUnpack);
  DoTest('TestQuintupleAssign',TestQuintupleAssign);
  DoTest('TestQuintupleUnpack',TestQuintupleUnpack);
end.

