{ type helpers in mode ObjFPC can also be used in place of record and class
  helpers }

program tthlp23;

{$mode objfpc}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

type
  TTest = record
    function Test: LongInt;
    class function TestStatic: LongInt; static;
  end;

  TTestHelper = type helper for TTest
    function Test: LongInt;
    class function TestStatic: LongInt; static;
  end;

  TTestHelperSub = type helper(TTestHelper) for TTest
    function Test: LongInt;
    class function TestStatic: LongInt; static;
  end;

  TObjectHelper = type helper for TObject
    function Test: LongInt;
    class function TestClass: LongInt;
    class function TestStatic: LongInt; static;
  end;

  TObjectHelperSub = type helper(TObjectHelper) for TObject
    function Test: LongInt;
    class function TestClass: LongInt;
    class function TestStatic: LongInt; static;
  end;

  TObjectSub = class(TObject)
    function Test: LongInt;
    class function TestClass: LongInt;
    class function TestStatic: LongInt; static;
  end;

  TObjectSubHelperSub = type helper(TObjectHelper) for TObjectSub
    function Test: LongInt;
    class function TestClass: LongInt;
    class function TestStatic: LongInt; static;
  end;

function TTest.Test: LongInt;
begin
  Result := 1;
end;

class function TTest.TestStatic: LongInt;
begin
  Result := 2;
end;

function TTestHelper.Test: LongInt;
begin
  Result := 3;
end;

class function TTestHelper.TestStatic: LongInt;
begin
  Result := 4;
end;

function TTestHelperSub.Test: LongInt;
begin
  Result := 5;
end;

class function TTestHelperSub.TestStatic: LongInt;
begin
  Result := 6;
end;

function TObjectHelper.Test: LongInt;
begin
  Result := 7;
end;

class function TObjectHelper.TestClass: LongInt;
begin
  Result := 8;
end;

class function TObjectHelper.TestStatic: LongInt;
begin
  Result := 9;
end;

function TObjectHelperSub.Test: LongInt;
begin
  Result := 10;
end;

class function TObjectHelperSub.TestClass: LongInt;
begin
  Result := 11;
end;

class function TObjectHelperSub.TestStatic: LongInt;
begin
  Result := 12;
end;

function TObjectSub.Test: LongInt;
begin
  Result := 13;
end;

class function TObjectSub.TestClass: LongInt;
begin
  Result := 14;
end;

class function TObjectSub.TestStatic: LongInt;
begin
  Result := 15;
end;

function TObjectSubHelperSub.Test: LongInt;
begin
  Result := 16;
end;

class function TObjectSubHelperSub.TestClass: LongInt;
begin
  Result := 17;
end;

class function TObjectSubHelperSub.TestStatic: LongInt;
begin
  Result := 18;
end;

var
  t: TTest;
  o: TObject;
  os: TObjectSub;
begin
  if t.Test <> 5 then
    Halt(1);
  if t.TestStatic <> 6 then
    Halt(2);
  if TTest.TestStatic <> 6 then
    Halt(3);
  o := TObject.Create;
  if o.Test <> 10 then
    Halt(4);
  if o.TestClass <> 11 then
    Halt(5);
  if o.TestStatic <> 12 then
    Halt(6);
  if TObject.TestClass <> 11 then
    Halt(7);
  if TObject.TestStatic <> 12 then
    Halt(8);
  os := TObjectSub.Create;
  if os.Test <> 16 then
    Halt(9);
  if os.TestClass <> 17 then
    Halt(10);
  if os.TestStatic <> 18 then
    Halt(11);
  if TObjectSub.TestClass <> 17 then
    Halt(12);
  if TObjectSub.TestStatic <> 18 then
    Halt(13);
  Writeln('ok');
end.
