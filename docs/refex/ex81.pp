Program example81;

{ Program to demonstrate the HexStr function }

Const Value = 45678;

Var I : longint;

begin
  For I:=1 to 10 do
    Writeln (HexStr(Value,I));
end.
