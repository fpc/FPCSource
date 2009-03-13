{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

type
  ta = class
    a: byte;
  end;

var
  a: ta;
  b: byte absolute a.a;
begin
end.

