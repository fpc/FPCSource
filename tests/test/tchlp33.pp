{ only the last available class helper for a class must be used - test 2 }
program tchlp33;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  uchlp33a, uchlp33c, uchlp33b;

var
  f: TFoo;
  res: Integer;
begin
  f := TFoo.Create;
  res := f.Test;
  Writeln('f.Test: ', res);
  if res <> 1 then
    Halt(1);
end.

