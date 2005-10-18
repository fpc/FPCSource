{ %fail }

{ Source provided for Free Pascal Bug Report 4445 }
{ Submitted by "lito steel" on  2005-10-17 }
{ e-mail: litosteel@yahoo.com }
{ those are the definitions }
const
  StackMax = 10;
var
       Stack:array[1..StackMax] of double;
       StackTop: integer = 0;

{in this procedure the compiler complains}
procedure aAND;
var a: double;
begin
  a := Stack[StackTop];
  dec(StackTop);
  Stack[StackTop] := Stack[StackTop] and a;
end;

begin
  aand;
end.
