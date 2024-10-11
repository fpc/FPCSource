program tb0717;

{$mode objfpc}
{$modeswitch advancedrecords}

uses
  ub0717;

type
  TTest = record
    i: LongInt;
    class operator Initialize(var t: TTest);
    class operator Finalize(var t: TTest);
  end;

class operator TTest.Initialize(var t: TTest);
begin
  t.i := 42;
end;

class operator TTest.Finalize(var t: TTest);
begin
  { if this isn't reached then the finalization section of ub0717 will exit
    with an error }
  TestOk := True;
end;

procedure Test;
var
  t: TTest;
begin
  if t.i <> 42 then
    Halt(1);
  t := Default(TTest);
  if t.i <> 42 then
    Halt(2);
end;

begin
  Test;
end.
