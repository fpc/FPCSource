{%FAIL}
{$mode objfpc}
{
  test undefined constants which must be typed
}
program tgenconst18;

type
	generic TUndefined<const U> = record end;

begin
end.
