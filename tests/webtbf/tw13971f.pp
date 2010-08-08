{ %fail }

{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  tc = class
    destructor destroy; override;
    a: longint;
  end;

destructor tc.destroy;
begin
end;

begin
end.
