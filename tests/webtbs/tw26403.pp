{ %OPT=-Sew }
{$OPTIMIZATION DFA}
{$HINTS ON}
program test;

type
   TIntArray = array of Integer;

procedure Reset(var Foo: TIntArray);
begin
   SetLength(Foo, 0);
end;

procedure Foo(var Bar: TIntArray);
begin
   Reset(Bar); // Hint: Local variable "Bar" does not seem to be initialized
end;

var
   Baz: TIntArray;
begin
   Foo(Baz); // Hint: Local variable "Baz" does not seem to be initialized
end.
