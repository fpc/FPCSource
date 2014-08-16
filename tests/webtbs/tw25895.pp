{$MODE OBJFPC}
program test;

type
   TDummy = record end;

function Foo(): TDummy;
begin
   Result := Default(TDummy);
end; // Fatal: Internal error 2010053111

begin
   Foo();
end.
