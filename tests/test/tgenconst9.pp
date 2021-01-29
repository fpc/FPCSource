{%FAIL}
{$mode objfpc}
{
  test type mismatch when specializing constants with types
}
program tgenconst9;
type
	generic TByte<const U: Byte> = record end;
var
	a: specialize TByte<string>;
begin
end.
