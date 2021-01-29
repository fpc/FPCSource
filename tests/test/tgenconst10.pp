{%FAIL}
{$mode objfpc}
{
  test type mismatch when specializing generic type with constant value
}
program tgenconst10;

type
	generic TByte<T> = record end;
	
var
	a: specialize TByte<10>;
begin
end.
