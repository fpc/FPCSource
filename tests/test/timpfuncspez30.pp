{%FAIL}
{$mode objfpc}
{$modeswitch implicitfunctionspecialization}
{
  All generic parameters must be used in the paramter list
}
program timpfuncspez30;

type
  generic TAnyRecord<U> = record
  end;

type
  TSomeRecord = specialize TAnyRecord<Integer>;

generic procedure DoThis<T>(aRecord: specialize TAnyRecord<Integer>);
begin
end;

var
  rec: TSomeRecord;
begin
  DoThis(rec);
end.