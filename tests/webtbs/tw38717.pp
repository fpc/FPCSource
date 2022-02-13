program CurrencyTest;

{$mode objfpc}{$H+}

var
  C: Currency;
  R, D: Currency;
begin
  C := 1234.56;
  R := 4;
  D := 2;
  C := C * R / D;
  Writeln(C);
end.

