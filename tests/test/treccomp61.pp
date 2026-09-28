{ %FAIL }
{ Record composition: "contains alias" requires an existing field, a type
  name is not allowed }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = record
    contains alias TChildRec;
  end;

begin
end.
