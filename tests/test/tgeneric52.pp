{ this tests that forced typecasts to inline specialized types work }
program tgeneric52;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest = class
    function Test: Integer;
  end;

  TTestGen<T> = class(TTest)
    function Test: Integer;
  end;

function TTest.Test: Integer;
begin
  Result := 1;
end;

function TTestGen<T>.Test: Integer;
begin
  Result := 2;
end;

var
  t: TTest;
  res: Integer;
begin
  t := TTestGen<Integer>.Create;
  res := TTestGen<Integer>(t).Test;
  Writeln('t.Test: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
