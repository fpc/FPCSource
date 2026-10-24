{ %FAIL }
{ %NORUN }
{ Record composition: in a specialization of a generic record with an unnamed
  composition of its type parameter, a field declared before the composition
  with the same name as a composed member is a duplicate identifier error }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  generic TComposed<T> = record
    A: Integer;
    B: Integer;
    contains T;
    C: Integer;
  end;

var
  c: specialize TComposed<TChildRec>;
begin
end.
