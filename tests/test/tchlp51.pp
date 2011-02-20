{ this tests whether a class helper introduced in the uses clause of an
  implementation section overrides the one introduced in the interface section }
program tchlp51;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$apptype console}

uses
  uchlp51a, uchlp51c;

var
  f: TFoo;
  res: Integer;
begin
  f := TFoo.Create;
  res := f.AccessTest;
  Writeln('f.AccessTest: ', res);
  if res <> 1 then
    Halt(1);
  Writeln('ok');
end.
