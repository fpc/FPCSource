{ test whether the correct class helper is used, if two are defined for the
  same class in a unit }
program tchlp50;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}
{$apptype console}

uses
  uchlp50;

var
  f: TFoo;
  res: Integer;
begin
  f := TFoo.Create;
  res := f.Test;
  Writeln('f.Test: ', res);
  if res <> 2 then
    Halt(1);
  Writeln('ok');
end.

