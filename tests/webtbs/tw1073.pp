
type Char4=array[1..4] of char;
     T1=packed record
      A1:Char4;
      A2:Char4;
      A3:Char4;
     end;
     PT2=^T2;
     T2=record
      B1:T1;
      B2:Char4;
      B3:longint;
     end;
     T3=record
      C1:Char4;
     end;

var S1,S2:String;

procedure trifich(P1,P2,P3:string; P4:boolean);
begin
  if P4 then WriteLn(P2+P3+'IN '+P1);
end;

var V1:PT2;
    V2:T3;
begin
  new(V1);
  s1 := 'abc';
  s2 := 'def';
  with  v1^ do
    begin
      b1.a1 := '1234';
      b1.a2 := '5678';
      b1.a3 := 'ghij';
      b2 := '0000';
      b3 := longint(char4('9999'));
    end;
  v2.c1 := 'wxyz';
  TriFich(S1+S2,
          V1^.B1.A1+V1^.B1.A2+V1^.B1.A3+V1^.B2+Char4(V1^.B3)+#13#10,
          V1^.B1.A1+V1^.B1.A2+V1^.B1.A3+V2.C1+Char4(V1^.B3)+#13#10,true);
end.
