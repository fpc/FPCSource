program tfuncref23;

{$mode objfpc}
{$modeswitch functionreferences}


type
  TLongIntFunc = reference to function(aArg: LongInt): LongInt;

  TTest = class
    i: LongInt;
    function TestCaptureSelf: TLongIntFunc;
  end;


function TestNoCapture: TLongIntFunc;

  function Foobar(aArg: LongInt): LongInt;
  begin
    Result := 42 * aArg;
  end;

begin
  Result := @Foobar;
end;

function TestCaptureLocal: TLongIntFunc;
var
  i: LongInt;

  function Foobar(aArg: LongInt): LongInt;
  begin
    Result := i * aArg;
  end;

begin
  i := 0;
  Result := @Foobar;
  i := 21;
end;

function TTest.TestCaptureSelf: TLongIntFunc;

  function Foobar(aArg: LongInt): LongInt;
  begin
    Result := i * aArg;
  end;

begin
  i := 0;
  Result := @Foobar;
  i := 84;
end;

var
  t: TTest;
  f: TLongIntFunc;
begin
  f := TestNoCapture();
  if f(2) <> 84 then
    Halt(1);

  f := TestCaptureLocal();
  if f(2) <> 42 then
    Halt(2);

  t := TTest.Create;
  try
    f := t.TestCaptureSelf;
    if f(2) <> 168 then
      Halt(3);
  finally
    t.Free;
  end;

  Writeln('ok');
end.
