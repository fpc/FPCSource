{ anonymous functions that don't capture any local variables can be assigned
  to function/procedure variables }

program tanonfunc4;

{$mode delphi}
{$modeswitch anonymousfunctions}

type
  TFunction = function(aArg: LongInt): LongInt;
  TProcedure = procedure(aArg: LongInt);

  TTest = class
    procedure Test;
  end;

var
  g: LongInt;

procedure TTest.Test;
var
  f: TFunction;
  p: TProcedure;
begin
  f := function(aArg: LongInt): LongInt begin Result := aArg; end;
  if f(42) <> 42 then
    Halt(5);
  p := procedure(aArg: LongInt) begin g := aArg; end;
  g := 0;
  p(42);
  if g <> 42 then
    Halt(6);
end;

procedure Test;
var
  f: TFunction;
  p: TProcedure;
begin
  f := function(aArg: LongInt): LongInt begin Result := aArg; end;
  if f(42) <> 42 then
    Halt(3);
  p := procedure(aArg: LongInt) begin g := aArg; end;
  g := 0;
  p(42);
  if g <> 42 then
    Halt(4);
end;

var
  f: TFunction;
  p: TProcedure;
  t: TTest;
begin
  f := function(aArg: LongInt): LongInt begin Result := aArg; end;
  if f(42) <> 42 then
    Halt(1);
  p := procedure(aArg: LongInt) begin g := aArg; end;
  g := 0;
  p(42);
  if g <> 42 then
    Halt(2);

  Test;

  t := TTest.Create;
  try
    t.Test;
  finally
    t.Free;
  end;
end.
