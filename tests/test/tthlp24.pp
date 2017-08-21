program tthlp24;

{$mode objfpc}
{$modeswitch typehelpers}

type
  ITestIntf = interface
    function Blubb: LongInt;
    function Foobar: LongInt;
  end;

  ITestIntfSub = interface(ITestIntf)
    function Something: LongInt;
  end;

  ITestIntfSubSub = interface(ITestIntfSub)
    function SomethingElse: LongInt;
  end;

  TTest = class(TInterfacedObject, ITestIntf)
    function Blubb: LongInt;
    function Foobar: LongInt;
  end;

  TTestSub = class(TTest, ITestIntfSub)
    function Something: LongInt;
  end;

  TTestSubSub = class(TTestSub, ITestIntfSubSub)
    function SomethingElse: LongInt;
  end;

  TTestIntfHelper = type helper for ITestIntf
    function Blubb: LongInt;
    function Foobar(aArg: LongInt): LongInt; overload;
    function Test: LongInt;
    class function TestStatic: LongInt; static;
  end;

  TTestIntfSubSubHelper = type helper(TTestIntfHelper) for ITestIntfSubSub
    function SomethingElse: LongInt;
  end;

{ TTestSubSub }

function TTestSubSub.SomethingElse: LongInt;
begin
  Result := 9;
end;

{ TTestSub }

function TTestSub.Something: LongInt;
begin
  Result := 8;
end;

{ TTestIntfSubSubHelper }

function TTestIntfSubSubHelper.SomethingElse: LongInt;
begin
  Result := 7;
end;

{ TTest }

function TTest.Blubb: LongInt;
begin
  Result := 4;
end;

function TTest.Foobar: LongInt;
begin
  Result := 5;
end;

{ TTestIntfHelper }

function TTestIntfHelper.Blubb: LongInt;
begin
  Result := 3;
end;

function TTestIntfHelper.Foobar(aArg: LongInt): LongInt;
begin
  Result := aArg;
end;

function TTestIntfHelper.Test: LongInt;
begin
  Result := 1;
end;

class function TTestIntfHelper.TestStatic: LongInt;
begin
  Result := 2;
end;

var
  i: ITestIntf;
  _is: ITestIntfSub;
  iss: ITestIntfSubSub;
begin
  i := TTest.Create;
  if i.Test <> 1 then
    Halt(1);
  if i.TestStatic <> 2 then
    Halt(2);
  if ITestIntf.TestStatic <> 2 then
    Halt(3);
  if i.Blubb <> 3 then
    Halt(4);
  if i.Foobar <> 5 then
    Halt(5);
  if i.Foobar(6) <> 6 then
    Halt(6);
  i := Nil;

  _is := TTestSub.Create;
  if _is.Blubb <> 3 then
    Halt(7);
  if _is.Foobar(8) <> 8 then
    Halt(8);
  _is := Nil;

  iss := TTestSubSub.Create;
  if iss.SomethingElse <> 7 then
    Halt(9);
  if iss.Blubb <> 3 then
    Halt(10);
  iss := Nil;

  Writeln('ok');
end.
