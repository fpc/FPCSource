{ %FAIL }

program tprocvar6;

{$mode delphi}

var
  Z: procedure of object;

type
  C = class
    procedure Foo;
  end;

procedure C.Foo;
begin
end;

begin
  Z := C.Foo;
end.
