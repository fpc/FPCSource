{ anonymous functions that capture nothing or Self can be assigned to method
  variables }

program tanonfunc6;

{$mode delphi}
{$modeswitch anonymousfunctions}

type
  TTestMethod = function(aArg: LongInt): LongInt of object;

  TTest = class
    f: LongInt;
    function Func: LongInt;
    procedure Test;
    property p1: LongInt read f;
    property p2: LongInt read Func;
  end;

procedure TTest.Test;
var
  tm: TTestMethod;
begin
  tm := function(aArg: LongInt): LongInt begin Result := aArg + 5; end;
  if tm(37) <> 42 then
    Halt(2);

  f := 2;
  tm := function(aArg: LongInt): LongInt begin Result := f * aArg; end;
  if tm(21) <> 42 then
    Halt(3);

  f := 3;
  tm := function(aArg: LongInt): LongInt begin Result := p1 * aArg; end;
  if tm(4) <> 12 then
    Halt(4);

  f := 4;
  tm := function(aArg: LongInt): LongInt begin Result := Func * aArg; end;
  if tm(5) <> 20 then
    Halt(5);

  f := 5;
  tm := function(aArg: LongInt): LongInt begin Result := p2 * aArg; end;
  if tm(3) <> 15 then
    Halt(6);
end;

function TTest.Func: LongInt;
begin
  Result := f;
end;

var
  t: TTest;
  tm: TTestMethod;
begin
  tm := function(aArg: LongInt): LongInt begin Result := aArg * 2; end;
  if tm(2) <> 4 then
    Halt(1);

  t := TTest.Create;
  try
    t.Test;
  finally
    t.Free;
  end;
end.
