{ this tests that type checks for inline specialized types work }
program tgeneric53;

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
begin
  t := TTestGen<Integer>.Create;
  if t is TTestGen<Integer> then
    Writeln('t is a TTestGen<Integer>')
  else begin
    Writeln('t is not a TTestGen<Integer>');
    Halt(1);
  end;
  Writeln('ok');
end.

