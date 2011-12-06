{ TTest<T, S> is used from one unit, while TTest<T> is used from another }
program tgeneric59;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

uses
  ugeneric59a,
  ugeneric59b;

type
  TTestInteger = TTest<Integer>;
  TTestIntegerString = TTest<Integer, String>;

var
  res: Integer;
begin
  res := TTestInteger.Test;
  Writeln('TTestInteger.Test: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.
