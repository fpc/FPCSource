program tthlp30;

{$mode objfpc}
{$modeswitch typehelpers}

type
  Test1 = type LongInt;
  Test2 = type LongInt;
  Test3 = type Test1;

  TLongIntHelper = type helper for LongInt
    function TestA: LongInt;
    function TestB: LongInt;
  end;

  TTest1Helper = type helper(TLongIntHelper) for Test1
    function TestA: LongInt;
  end;

  TTest2Helper = type helper(TLongIntHelper) for Test2
    function TestB: LongInt;
  end;

  TTest3Helper = type helper(TLongIntHelper) for Test3
  end;

function TTest2Helper.TestB: LongInt;
begin
  Result := 2;
end;

function TTest1Helper.TestA: LongInt;
begin
  Result := 2;
end;

function TLongIntHelper.TestA: LongInt;
begin
  Result := 1;
end;

function TLongIntHelper.TestB: LongInt;
begin
  Result := 1;
end;

var
  l: LongInt;
  t1: Test1;
  t2: Test2;
  t3: Test3;
begin
  if l.TestA <> 1 then
    Halt(1);
  if l.TestB <> 1 then
    Halt(2);

  if t1.TestA <> 2 then
    Halt(3);
  if t1.TestB <> 1 then
    Halt(4);

  if t2.TestA <> 1 then
    Halt(5);
  if t2.TestB <> 2 then
    Halt(6);

  if t3.TestA <> 1 then
    Halt(7);
  if t3.TestB <> 1 then
    Halt(8);
end.
