{%NORUN}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  Test specializing related object types

  DoThis<T> can be specialized as DoThis<Integer>
  because TSomeRecord is specialized from TAnyRecord<Integer>
  and therefore we can infer "Integer" as the correct parameter
}
program timpfuncspez20;

type
  generic TAnyRecord<U> = record
  end;

type
  TSomeRecord = specialize TAnyRecord<Integer>;

generic procedure DoThis<T>(aRecord: specialize TAnyRecord<T>);
begin
end;

var
  rec: TSomeRecord;
begin
  DoThis(rec);
end.