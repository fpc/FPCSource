{ %FAIL }

program tprocvar10;

{$mode delphi}

var
  Z: procedure of object;

type
  C = class
  end;

  H = class helper for C
    procedure Foo;
    class procedure ClassCtx;
  end;

procedure H.Foo;
begin
end;

class procedure H.ClassCtx;
begin
  Z := Foo;
end;

begin
end.
