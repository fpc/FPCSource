{ %FAIL }
{ Should not give an internal error anymore }
{ Bug report submitted by Alexander S. Klenin }
{ 2012-03-26 }

begin
  Writeln(1/Abs(1) mod 1);
end.
