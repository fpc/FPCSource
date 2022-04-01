{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Test that sets are not compatible with open arrays
}

program timpfuncspez3;

generic procedure DoThis<T>(a: array of T);
begin
end;

const
	c1 = [1,2,3];
begin
	DoThis(c1);
end.