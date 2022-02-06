{ any anonymous function can be assigned to a nested procedure variable }

program tanonfunc8;

{$mode delphi}
{$modeswitch anonymousfunctions}
{$modeswitch nestedprocvars}

type
  TTestFunc = function(aArg: LongInt): LongInt is nested;

type
  TTest = class
    f: LongInt;
    procedure Test;
  end;

procedure TTest.Test;
var
  tf: TTestFunc;
  i: LongInt;
begin
  tf := function(aArg: LongInt): LongInt begin Result := aArg * 2; end;
  if tf(2) <> 4 then
    Halt(5);

  tf := function(aArg: LongInt): LongInt begin Result := aArg * f; end;

  f := 3;
  if tf(2) <> 6 then
    Halt(6);

  f := 4;
  if tf(2) <> 8 then
    Halt(7);

  tf := function(aArg: LongInt): LongInt begin Result := aArg * (f + i); end;

  f := 4;
  i := 1;

  if tf(2) <> 10 then
    Halt(8);

  f := 5;
  i := 1;

  if tf(2) <> 12 then
    Halt(9);

  f := 5;
  i := 2;

  if tf(2) <> 14 then
    Halt(10);
end;

procedure Test;
var
  tf: TTestFunc;
  i: LongInt;
begin
  tf := function(aArg: LongInt): LongInt begin Result := aArg * 2; end;
  if tf(2) <> 4 then
    Halt(2);

  tf := function(aArg: LongInt): LongInt begin Result := aArg * i; end;

  i := 3;
  if tf(2) <> 6 then
    Halt(3);

  i := 4;
  if tf(2) <> 8 then
    Halt(4);
end;

var
  tf: TTestFunc;
begin
  tf := function(aArg: LongInt): LongInt begin Result := aArg * 2; end;
  if tf(1) <> 2 then
    Halt(1);

  Test;
end.
