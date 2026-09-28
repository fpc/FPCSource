{ %FAIL }
{ Record composition: a member of an unnamed composed record with the same
  name as a field of the composing record is a duplicate identifier error }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = record
    A: Integer;
    B: Integer;
    contains TChildRec;
    C: Integer;
  end;

begin
end.
