{ %version=1.1}
{ %fail }

{ Source provided for Free Pascal Bug Report 2128 }
{ Submitted by "Bill Rayer" on  2002-09-18 }
{ e-mail: lingolanguage@hotmail.com }

{
  Excessive 64-bit literal causes the FPC compiler to crash.
}
var
  c : comp;
begin
  c := -9223372036854775809;
  if c<>-9223372036854775809 then
    halt(1);
end.
