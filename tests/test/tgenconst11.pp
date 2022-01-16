{%FAIL}
{$mode objfpc}
{
	test def compare fail with specialized types
}
program tgenconst11;
type
	generic TConst<const U: integer> = class end;
var
	a:specialize TConst<10>;
begin
	a:=specialize TConst<'string'>.Create;
end