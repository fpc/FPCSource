{ %FAIL }

program tprocvar9;

{$mode delphi}

var
  Z: procedure of object;

type
  C = class
  end;

  H = class helper for C
    procedure Foo;
  end;

procedure H.Foo;
begin
end;

begin
  Z := H.Foo;
end.
