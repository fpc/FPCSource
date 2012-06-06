{ %opt=-CO -vh -Seh }
{ %norun }
program tw19581;

type
   TFoo = 0..1000;
var
   Foo: TFoo;
begin
   Foo:=999;
   writeln(Foo);
end.

