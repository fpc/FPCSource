{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Non-uniform array constructors are not compatible with "array of T"
}

program timpfuncspez33;

generic procedure DoThis<T>(a: array of T);
begin
end;

begin
  DoThis([
    1,
    'a',
    TObject.Create
  ]);
end.