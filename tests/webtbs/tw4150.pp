{$mode objfpc}{$r+}{$q+}
var
Prec:Cardinal;
Bits:Word;
begin
  Bits := 512;
  Prec := 8;
  Bits := Bits shr (16-Prec);
  WriteLn(bits);
end.

