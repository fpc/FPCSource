{ %NORUN }
{$mode objfpc}
{
  test lists of types/contants
}
program tgenconst2;

type
	generic TMoreThanOne<T1,T2;const U1,U2:integer> = record end;
	
var
	a: specialize TMoreThanOne<integer,string,10,10>;
begin
end.
