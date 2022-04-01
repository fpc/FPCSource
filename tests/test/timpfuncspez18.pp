{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Constants with generics are excluded
}

program timpfuncspez18;

generic procedure DoThis<T; const U: integer>(a: T);
begin
  writeln(U);
end;

begin
  DoThis(1);
end.