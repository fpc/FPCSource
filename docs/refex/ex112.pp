Program example112;

{ Program to demonstrate the OctStr function }

Const Value = 45678;

Var I : longint;

begin
  For I:=1 to 10 do
    Writeln (OctStr(Value,I));
  For I:=1 to 16 do
    Writeln (OctStr(I,3));
end.
