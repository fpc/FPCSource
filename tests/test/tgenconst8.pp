{%FAIL}
{$mode objfpc}
{
  test out of range error with constants
}
program tgenconst8;

type
	generic TByte<const U: Byte> = record end;
	
var
	a: specialize TByte<300>;
begin
end.
