program tw40928;

{$mode OBJFPC}
{$H+}

uses
  SysUtils;

var
  j: currency;
  i: int64;
  k: Double;

begin
  j:=9500000.0004;
  k:=10000.0;
  i:=Round(j * k);
  if i<>95000000004 then // Does compiler avoid an overflow?
    Halt(1);
  WriteLn('ok');
end.
