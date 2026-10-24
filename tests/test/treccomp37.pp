{ %FAIL }
{ Record composition: specializing a generic record composing its type
  parameter with a non-record type (Integer) is not allowed }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  generic TComposed<T> = record
    A: Integer;
    B: Integer;
    contains child: T;
    D: Integer;
  end;

var
  c: specialize TComposed<Integer>;
begin
end.
