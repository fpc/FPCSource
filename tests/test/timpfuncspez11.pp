{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
	Testing ambiguous specializations
}

program timpfuncspez11;

generic procedure DoThis<T>(a:T; b: word);
begin
end;

generic procedure DoThis<T>(a: word; b: T);
begin
end;

begin
	DoThis(1,1);
end.