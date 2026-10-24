{ %FAIL }
{ %NORUN }
{ Record composition: in a generic record, the name of a named composition
  must not duplicate an existing field }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  generic TComposed<T> = record
    A: Integer;
    contains A: T;
    C: Integer;
  end;

var
  c: specialize TComposed<TChildRec>;
begin
end.
