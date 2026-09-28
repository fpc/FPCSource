{ %FAIL }
{ Record composition: a field of the composing record declared after an unnamed
  composition with the same name as a composed member is a duplicate identifier error }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    C: Integer;
  end;

  TComposed = record
    A: Integer;
    B: Integer;
    contains TChildRec;
    C: Integer;
  end;

begin
end.
