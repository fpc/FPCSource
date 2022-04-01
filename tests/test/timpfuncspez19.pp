{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test specializing related object types

  DoThis<T> can be specialized as DoThis<Integer>
  because TSomeClass is specialized from TAnyClass<Integer>
  and therefore we can infer "Integer" as the correct parameter
}
program timpfuncspez19;

type
  generic TAnyClass<U> = class
  end;

type
  TSomeClass = specialize TAnyClass<Integer>;

generic procedure DoThis<T>(aClass: specialize TAnyClass<T>);
begin
end;

begin
  DoThis(TSomeClass.Create);
end.