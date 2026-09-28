{ %FAIL }
{ Record composition: "contains" is only supported in records, not in objects }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = object
    contains child: TChildRec;
  end;

begin
end.
