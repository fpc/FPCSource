{ %FAIL }

program tprocvar8;

{$mode delphi}

var
  Z: procedure of object;

type
  C = class
    procedure Foo;
  end;

  CC = class of C;

procedure C.Foo;
begin
end;

var
  aCC: CC = Nil;

begin
  Z := aCC.Foo;
end.
