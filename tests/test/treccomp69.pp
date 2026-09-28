{ Record composition: without modeswitch RecordComposition "contains" can be
  used as a field name }
program record_compose_test;

{$Mode ObjFPC}{$H+}

type
  TTest = record
    contains: Integer;
  end;

begin
end.
