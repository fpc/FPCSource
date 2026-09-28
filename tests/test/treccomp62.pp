{ %FAIL }
{ Record composition: "contains alias" cannot declare a new field, only
  reference an existing one }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TChildRec = record
    B: Integer;
  end;

  TComposed = record
    contains alias child: TChildRec;
  end;

begin
end.
