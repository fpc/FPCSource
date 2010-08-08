{ %wpoparas=devirtcalls,optvmts }
{ %wpopasses=1 }

{$mode objfpc}

{ same as two1, except with a unit to test loading wpo info from a ppu file }

uses
  uwpo2;

var
  a: ta;
  ca: class of ta;
begin
  tb.test2;
  a:=cc.create;
  a.test;
  a.free
end.
