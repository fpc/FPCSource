{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  ta = record
    a: byte;
  end;
  pa = ^ta;

var
  a: pa;
  b: byte absolute a.a;
begin
end.

