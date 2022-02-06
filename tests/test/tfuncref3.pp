{ %NORUN }

{ function references can also be declared as anonymous types }
program tfuncref3;

{$mode objfpc}
{$modeswitch functionreferences}

var
  Proc1: reference to procedure;

type
  TTestRecord = record
    Field1: reference to procedure;
  end;

  TTestObject = class
    Field1: reference to procedure;
  end;

var
  testvar,
  testuse: LongInt;

procedure TestProc;
begin
  testvar := testuse;
end;

var
  r: TTestRecord;
  o: TTestObject;
begin
  Proc1 := @TestProc;
  testuse := 42;
  Proc1();
  if testvar <> 42 then
    Halt(1);

  r.Field1 := @TestProc;
  testuse := 21;
  r.Field1();
  if testvar <> 21 then
    Halt(2);

  o := TTestObject.Create;
  o.Field1 := @TestProc;
  testuse := 84;
  o.Field1();
  if testvar <> 84 then
    Halt(3);
end.
