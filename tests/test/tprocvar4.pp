{ %FAIL }

program tprocvar4;

{$mode delphi}

var
  Z: procedure of object;

type
  R = record
    procedure Foo;
  end;

procedure R.Foo;
begin
end;

begin
  Z := R.Foo;
end.
