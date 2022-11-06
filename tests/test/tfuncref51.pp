program tfuncref51;

{$mode objfpc}
{$modeswitch functionreferences}

type
  TFunc = reference to function(aArg: LongInt): LongInt;

function Test(aArg: TFunc): LongInt;
begin
  Result := aArg(42);
end;

type
  TTest = class
    function Method(aArg: String): LongInt; overload;
    function Method(aArg: LongInt): LongInt; overload;
    procedure DoTest;
  end;

function TTest.Method(aArg: String): LongInt;
begin
  Result := 1;
end;

function TTest.Method(aArg: LongInt): LongInt;
begin
  Result := 2;
end;

procedure TTest.DoTest;
begin
  Test(@Method);
end;

function Func(aArg: String): LongInt; overload;
begin
  Result := 1;
end;

function Func(aArg: LongInt): LongInt; overload;
begin
  Result := 2;
end;

var
  t: TTest;
begin
  t := TTest.Create;
  t.DoTest;
  t.Free;
  if Test(@Func) <> 2 then
    Halt(2);
end.
