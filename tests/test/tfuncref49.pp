program tfuncref49;

{$mode delphi}
{$modeswitch functionreferences}
{$modeswitch nestedprocvars}

{same as tfuncref33 but with mode delphi}

type
  TProcVar = procedure;
  TMethodVar = procedure of object;
  TProcRef = reference to procedure;
  TNestedVar = procedure is nested;

  TTest = class
    function Test1(aArg: TProcVar): LongInt; overload;
    function Test1(aArg: TProcRef): LongInt; overload;

    function Test2(aArg: TMethodVar): LongInt; overload;
    function Test2(aArg: TProcRef): LongInt; overload;

    function Test3(aArg: TNestedVar): LongInt; overload;
    function Test3(aArg: TProcRef): LongInt; overload;

    function Test4(aArg: TProcVar): LongInt; overload;
    function Test4(aArg: TMethodVar): LongInt; overload;
    function Test4(aArg: TProcRef): LongInt; overload;

    function Test5(aArg: TProcVar): LongInt; overload;
    function Test5(aArg: TMethodVar): LongInt; overload;
    function Test5(aArg: TNestedVar): LongInt; overload;
    function Test5(aArg: TProcRef): LongInt; overload;

    procedure TestMethod;

    procedure DoTest;
  end;

procedure TestProc;
begin
end;

function TTest.Test1(aArg: TProcVar): LongInt;
begin
  Result := 1;
end;
function TTest.Test1(aArg: TProcRef): LongInt;
begin
  Result := 2;
end;
function TTest.Test2(aArg: TMethodVar): LongInt;
begin
  Result := 3;
end;
function TTest.Test2(aArg: TProcRef): LongInt;
begin
  Result := 4;
end;
function TTest.Test3(aArg: TNestedVar): LongInt;
begin
  Result := 5;
end;
function TTest.Test3(aArg: TProcRef): LongInt;
begin
  Result := 6;
end;
function TTest.Test4(aArg: TProcVar): LongInt;
begin
  Result := 7;
end;
function TTest.Test4(aArg: TMethodVar): LongInt;
begin
  Result := 8;
end;
function TTest.Test4(aArg: TProcRef): LongInt;
begin
  Result := 9;
end;
function TTest.Test5(aArg: TProcVar): LongInt;
begin
  Result := 10;
end;
function TTest.Test5(aArg: TMethodVar): LongInt;
begin
  Result := 11;
end;
function TTest.Test5(aArg: TNestedVar): LongInt;
begin
  Result := 12;
end;
function TTest.Test5(aArg: TProcRef): LongInt;
begin
  Result := 13;
end;

procedure TTest.TestMethod;
begin
end;

procedure TTest.DoTest;

  procedure NestedProc;
  begin
  end;

var
  f: TProcRef;
begin
  if Test1(TestProc) <> 1 then
    Halt(1);
  if Test1(TestMethod) <> 2 then
    Halt(2);
  if Test1(NestedProc) <> 2 then
    Halt(3);
  if Test1(f) <> 2 then
    Halt(4);

  if Test2(TestProc) <> 4 then
    Halt(5);
  if Test2(TestMethod) <> 3 then
    Halt(6);
  if Test2(NestedProc) <> 4 then
    Halt(7);
  if Test2(f) <> 4 then
    Halt(8);

  if Test3(TestProc) <> 5 then
    Halt(9);
  if Test3(TestMethod) <> 6 then
    Halt(10);
  if Test3(NestedProc) <> 5 then
    Halt(11);
  if Test3(f) <> 6 then
    Halt(12);

  if Test4(TestProc) <> 7 then
    Halt(13);
  if Test4(TestMethod) <> 8 then
    Halt(14);
  if Test4(NestedProc) <> 9 then
    Halt(15);
  if Test4(f) <> 9 then
    Halt(16);

  if Test5(TestProc) <> 10 then
    Halt(17);
  if Test5(TestMethod) <> 11 then
    Halt(18);
  if Test5(NestedProc) <> 12 then
    Halt(19);
  if Test5(f) <> 13 then
    Halt(20);
end;

var
  t: TTest;
begin
  t := TTest.Create;
  t.DoTest;
  t.Free;
end.
