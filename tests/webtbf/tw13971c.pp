{ %fail }

{$ifdef fpc}
{$mode objfpc}
{$endif}

type
  tc = class
    procedure test;
    register: longint;
  end;

procedure tc.test;
begin
end;

begin
end.

