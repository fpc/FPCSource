program tprocvar16;

{$mode delphi}

type C = class
    class procedure Foo;
end;
class procedure C.Foo; begin end;

type CC = class of C;

var Z: procedure of object;
begin
    Z := CC.Foo;
    if TMethod(Z).Code <> @C.Foo then
      Halt(1);
    if TMethod(Z).Data <> Pointer(C) then
      Halt(2);
end.
