{$mode objfpc}{$H+}

uses
  FmtBCD;

var
  bcd: tbcd;
  li: integer;
begin
  bcd := -100;
  li := bcd * 2;
  if li<>-200 then
    Halt(1);
end.
