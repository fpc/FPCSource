{ %NORUN }
{$mode objfpc}
{
  test def compare with specialized types
}
program tgenconst12;

type
  generic TTest<const U: integer> = class
  end;

type
	ATest = specialize TTest<100>;
begin 
end.
