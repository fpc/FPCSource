{ %target=darwin }

{ Objective C types are implicit pointer types -> nil pointer }
program tdefault15;

{$mode objfpc}
{$modeswitch objectivec1}

type
  TTest = objcclass
  end;
  TTestProto = objcprotocol
  end;

var
  t: TTest;
  tp: TTestProto;
begin
  t := Default(TTest);
  if assigned(t) then
    halt(1);
  tp := Default(TTestProto);
  if assigned(tp) then
    halt(2);
end.
