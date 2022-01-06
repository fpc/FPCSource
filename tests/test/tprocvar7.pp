{ %FAIL }

program tprocvar7;

{$mode delphi}

var
  Z: procedure of object;

type
  C = class
    procedure Foo;
    class procedure ClassCtx;
  end;

procedure C.Foo;
begin
end;

class procedure C.ClassCtx;
begin
  Z := Foo;
end;

begin
end.
