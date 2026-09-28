{ %FAIL }
{ Record composition: with modeswitch RecordComposition "contains" is a keyword
  in records and cannot be used as a field name }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch RecordComposition}

type
  TTest = record
    contains: Integer;
  end;

begin
end.
