program tanonfunc69;

{$mode delphi}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch nestedprocvars}

{same as tanonfunc56 but mode delphi}

type
  TTestProc = procedure;
  TTestProcRef = reference to procedure;
  TTestMethod = procedure of object;
  TTestNested = procedure is nested;

  TTest = class
    f: LongInt;

    function Test1(aArg: TTestProc): LongInt; overload;
    function Test1(aArg: TTestMethod): LongInt; overload;
    function Test1(aArg: TTestNested): LongInt; overload;

    function Test2(aArg: TTestProc): LongInt; overload;
    function Test2(aArg: TTestMethod): LongInt; overload;
    function Test2(aArg: TTestProcRef): LongInt; overload;

    function Test3(aArg: TTestProc): LongInt; overload;
    function Test3(aArg: TTestMethod): LongInt; overload;
    function Test3(aArg: TTestProcRef): LongInt; overload;
    function Test3(aArg: TTestNested): LongInt; overload;

    procedure DoTest;
  end;

function TTest.Test1(aArg: TTestProc): LongInt;
begin
  Result := 1;
end;

function TTest.Test1(aArg: TTestMethod): LongInt;
begin
  Result := 2;
end;

function TTest.Test1(aArg: TTestNested): LongInt;
begin
  Result := 3;
end;

function TTest.Test2(aArg: TTestProc): LongInt;
begin
  Result := 1;
end;

function TTest.Test2(aArg: TTestMethod): LongInt;
begin
  Result := 2;
end;

function TTest.Test2(aArg: TTestProcRef): LongInt;
begin
  Result := 3;
end;

function TTest.Test3(aArg: TTestProc): LongInt;
begin
  Result := 1;
end;

function TTest.Test3(aArg: TTestMethod): LongInt;
begin
  Result := 2;
end;

function TTest.Test3(aArg: TTestProcRef): LongInt;
begin
  Result := 3;
end;

function TTest.Test3(aArg: TTestNested): LongInt;
begin
  Result := 4;
end;

procedure TTest.DoTest;
var
  l: LongInt;
begin
  if Test1(procedure begin end) <> 1 then
    Halt(1);
  if Test1(procedure begin f := 42; end) <> 2 then
    Halt(2);
  if Test1(procedure begin l := 42; end) <> 3 then
    Halt(3);

  if Test2(procedure begin end) <> 1 then
    Halt(4);
  if Test2(procedure begin f := 42; end) <> 2 then
    Halt(5);
  if Test2(procedure begin l := 42; end) <> 3 then
    Halt(6);

  if Test3(procedure begin end) <> 1 then
    Halt(7);
  if Test3(procedure begin f := 42; end) <> 2 then
    Halt(8);
  if Test3(procedure begin l := 42; end) <> 3 then
    Halt(9);
  if Test3(TTestNested(procedure begin l := 42; end)) <> 4 then
    Halt(10);
end;

var
  t: TTest;
begin
  t := TTest.Create;
  try
    t.DoTest;
  finally
    t.Free;
  end;
end.
