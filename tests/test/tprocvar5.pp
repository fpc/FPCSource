{ %FAIL }

program tprocvar5;

{$mode delphi}

var
  Z: procedure of object;

type
  O = object
    procedure Foo;
  end;

procedure O.Foo;
begin
end;

begin
  Z := O.Foo;
end.
