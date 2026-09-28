{ %FAIL }
{ Record composition: the name of a named composition must not duplicate
  an existing field of the composing record }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = record
    A: Integer;
    contains A: TChildRec;
    C: Integer;
  end;

begin
end.
